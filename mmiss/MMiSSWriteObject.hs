{- This module contains code for writing a value of type MMiSSObject.
   This can be old or new and can be edited or imported.
   -}
module MMiSSWriteObject(
   writeToMMiSSObject,
   ) where

#include "config.h"

import Maybe

import qualified Control.Exception
import System.IO.Unsafe(unsafeInterleaveIO)

import Computation
import ExtendedPrelude
import AtomString
import Delayer
import VariableSet (toKey,emptyVariableSetSource)
import VariableSetBlocker
import Sources
import ReferenceCount
import Thread(mapMConcurrentExcep)


import BSem

import DialogWin

import Graph(ArcType)

import Link
import ObjectTypes(WrappedLink(..))
import View
import Folders
import EntityNames
import LinkManager
import LinkDrawer
import SpecialNodeActions

#if HAXMLINT
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Validate
#else
import XmlParse
import XmlTypes

import XmlValidate
#endif

import MMiSSDTDAssumptions
import MMiSSDTD
import MMiSSContent
import MMiSSVariant
import MMiSSVariantObject
import MMiSSObjectType
import MMiSSPreamble
import MMiSSPreObjects
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSObjectTypeInstance
import MMiSSEditXml (toExportableXml)
import MMiSSPackageFolder

---
-- Creates or update an object from an Xml Element.
-- If it fails it returns a String.
-- The MMiSSObjectType is the expected type of the object.
--
-- If the bool checkThisEditLock is set we insist on getting the edit lock
-- of the top object, if it already exists.  In any case we get the edit lock
-- of all included objects.
--
-- The package folder supplied together with the element's label specifies 
-- the object's new location.
-- Thus if the linked object is A and the label is B.C, the eventual location
-- will be A.B.C.  If an expectedLabel is supplied, this is checked against
-- the element's label, but it does not otherwise influence the function.
--
-- writeToMMiSSObject returns two things if successful. 
-- (1) a link to the new object; (2) if we split up the input element into
-- multiple (more than one) output elements, we return the top element,
-- namely the one that actually got written to the object.  (This is used
-- to update the XEmacs buffer when we write an object that has new included
-- objects.)
writeToMMiSSObject :: MMiSSObjectType -> View 
   -> MMiSSPackageFolder -> Maybe EntitySearchName -> Element -> Bool 
   -> IO (WithError (Link MMiSSObject,Maybe Element))
writeToMMiSSObject objectType view packageFolder 
   expectedLabel element checkThisEditLock =
 
  addFallOutWE (\ break ->
      (do
         -- (1) validate it.
         case validateElement (xmlTag objectType) element of
            (s@ (_:_)) -> break (unlines s)
            _ -> done

         -- (2) structure it
         let
            (contentsWE :: WithError StructuredContent) 
               = structureContents element

         (contents :: StructuredContent) 
            <- coerceWithErrorOrBreakIO break contentsWE

         let
            -- This is the function we use for getting the children of
            -- a StructuredContent object.
            childs object = children (accContents object)

            -- (3) get and check the object's label
            objectLabel :: EntitySearchName
            objectLabel = label contents

         case expectedLabel of
            Nothing -> done
            Just lab -> 
               if lab == objectLabel
                  then
                     done
                  else
                     break ("Expected label is "++toString lab++" but "
                        ++toString objectLabel++" found")

         -- (4) Get the ObjectLoc for the head object.  This is used
         -- (a) because if that doesn't work we might as well abort now.
         -- (b) we can use it to identify the head object in the list of
         --     PreObjects.
         headObjectLocOpt <- getObjectLoc view break packageFolder objectLabel
         headObjectLoc <- case headObjectLocOpt of
            Just headObjectLoc -> return headObjectLoc
            Nothing -> break ("Unable to write to " ++ toString objectLabel
               ++ " because package folder not found")

         -- (5) Construct PreObjects for contents.
         -- We use treeFoldM, with type variables 
         --    ancestorInfo = MMiSSPackageFolder
         --    state = PreObjects
         --    node = StructureContent
         --    mm = IO
         let
            treeFoldFn :: MMiSSPackageFolder -> PreObjects -> StructuredContent
               -> IO (MMiSSPackageFolder,PreObjects,[StructuredContent])
            treeFoldFn packageFolder0 preObjects0 structuredContent =
               do
                  let
                     searchName = label structuredContent
                  objectLocOpt 
                     <- getObjectLoc view break packageFolder searchName 
                  case objectLocOpt of
                     Nothing -> -- can't insert this object at all
                        return (packageFolder0,preObjects0,[])
                     Just objectLoc ->
                        do
                           let
                              preObjects1WE = addObject objectLoc 
                                 structuredContent preObjects0
                           preObjects1 
                              <- coerceWithErrorOrBreakIO break preObjects1WE
                           return (package objectLoc,preObjects1,
                              childs structuredContent)

         preObjects 
            <- treeFoldM treeFoldFn packageFolder emptyPreObjects contents
         let
            preObjectsWithoutHead = 
               removeObject headObjectLoc contents preObjects

         -- (6) Check that the objects have appropriate tags
         unitWE <- checkPreObjectTags view preObjects
         coerceWithErrorOrBreakIO break unitWE

         -- (7) Get the edit locks for the objects, excluding the head object
         -- if checkThisEditLock is set.
         let
            preObjectsToLock = 
               if checkThisEditLock
                  then
                     preObjects
                  else
                     preObjectsWithoutHead

         releaseActWE <- acquireEditLocks view preObjectsToLock
         releaseAct <- coerceWithErrorOrBreakIO break releaseActWE

         -- (8) Add all objects.
         headLink <- synchronizeView view (
            Control.Exception.finally (
               do
                  mapMConcurrentExcep
                     (simpleWriteToMMiSSObject view break) 
                     (listPreObjects preObjectsWithoutHead)
                  simpleWriteToMMiSSObject view break 
                     (headObjectLoc,[contents])
               )
               releaseAct
            )
    

         -- Compute the value to return
         let
            newElementOpt = case children (accContents contents) of
               [] -> Nothing
               _ -> Just (toTopElement contents)

         -- (9) return a link to the head object
         return (headLink,newElementOpt)
      ))


---
-- Create a single object.  The contents list must be non-empty.
simpleWriteToMMiSSObject :: View -> BreakFn 
   -> (ObjectLoc,[StructuredContent])
   -> IO (Link MMiSSObject)
simpleWriteToMMiSSObject view break (objectLoc,contentsList) =
   do
      -- (1) Construct the object, if necessary.
      -- This does not actually insert the object in the folder, the reason
      -- being that we want to hide the object from inspection by the display
      -- routine until something has been put in its variant dictionary.
      -- Instead we return "afterAct", which does precisely that.
      -- We also return an "dirty" action which, for old objects, dirties it
      -- (indicating it has changed), and the object's name in the object.
      (objectLink,object,dirtyAct :: IO (),afterAct :: IO (WithError ()))
            <- case ifExists objectLoc of
         Just objectLink ->
            do
               versioned <- fetchLink view objectLink
               object <- readObject view versioned
               return (objectLink,object,dirtyObject view versioned,
                  return (hasValue ()))
         Nothing ->
            do
               let
                  parentLinkedObject = toLinkedObject (package objectLoc)
                  entityName = name objectLoc

                  mmissObjectType 
                     = retrieveObjectType (tag (head contentsList))

               nodeActions <- newNodeActionSource
               extraNodes <- newBlocker emptyVariableSetSource

               creationResult <- createLinkedObjectChildSplit 
                  view parentLinkedObject entityName 
                  (\ linkedObject ->
                     do
                        variantObject <- newEmptyVariantObject
                           (converter view linkedObject)
                        editCount <- newRefCount
                        let
                           object = MMiSSObject {
                              mmissObjectType = mmissObjectType,
                              linkedObject = linkedObject,
                              nodeActions = nodeActions,
                              variantObject = variantObject,
                              extraNodes = extraNodes,
                              editCount = editCount
                              }
                        return object
                     )

               let 
                  (objectLink,afterAct) = fromMaybe 
                     (break "Couldn't insert object in folder")
                     creationResult

               seq objectLink done

               object <- readLink view objectLink
               return (objectLink,object,done,afterAct)


      let
         -- (2) Insert the variants in the object
         ---
         -- Split the contents.  contents1 is distinguished because we make it
         -- the current variant.  (Yes, this is somewhat random.)
         contents1 : contentsRest = contentsList

         varObject = variantObject object

         ---
         -- Function for inserting a contents item.  The Bool indicates
         -- whether we make it the current item or not.
         insertItem :: Bool -> StructuredContent -> IO ()
         insertItem doPoint content =
            do
               let
                  element0 = toTopElement content 
                  element = delLabel element0

               oldVariableOpt <- lookupVariantObjectExact varObject
                  (variantSpec content)
               case oldVariableOpt of
                  Just (variable @ Variable {element = elementLink}) ->
                     do
                        -- The only thing we do is write the new element to
                        -- the link and, if necessary, make it current.
                        writeLink view elementLink element
                        if doPoint 
                           then 
                               pointVariantObject varObject 
                                  (variantSpec content) 
                           else
                               done

                  Nothing ->
                     do
                        -- Create the new object
                        elementLink <- createLink view element
                        editLock <- newBSem
                        let
                           variable = Variable {
                              element = elementLink,
                              editLock = editLock
                              }

                           write = if doPoint then writeVariantObjectAndPoint 
                              else writeVariantObject

                        write varObject (variantSpec content) variable

                        -- Dirty the object
                        dirtyAct

      insertItem True contents1
      mapM (insertItem False) contentsRest

      -- (4) Insert the object into the folder.  If this somehow fails
      -- we delete the link, and break.
      insertResult <- afterAct
      case fromWithError insertResult of
         Right () -> done
         Left mess ->
            do
               deleteLink view objectLink
               break mess

      -- (5) return
      return objectLink

-- ---------------------------------------------------------------------
-- Some small helpful utilities
-- ---------------------------------------------------------------------

-- Get the objectLoc for an object about to be inserted or
-- modified.
getObjectLoc :: View -> BreakFn -> MMiSSPackageFolder -> EntitySearchName
   -> IO (Maybe ObjectLoc)
getObjectLoc view break packageFolder searchName =
   do
      objectLinkOptWE <- lookupMMiSSObject view packageFolder searchName
      case fromWithError objectLinkOptWE of
         Left mess -> -- type error
            break ("Can't insert " ++ toString searchName
               ++ " because non-MMiSS object with same "
               ++ " name in path")
         Right Nothing -> 
            -- not found
            -- try getting a package directory by looking up one
            -- level.
            do
               (parentSearchName,thisName) <-
                  case searchNameDirBase searchName of
                     Just (parentSearchName,Just thisName)
                        -> return (parentSearchName,thisName)
                     Nothing -> break (
                        "Attempt to insert object in "
                        ++ "illegal place " ++ toString searchName
                        )
               mmissPackageFolderLinkOptWE 
                  <- lookupMMiSSPackageFolder view packageFolder 
                     parentSearchName
               mmissPackageFolderLinkOpt <- case fromWithError
                     mmissPackageFolderLinkOptWE of
                  Left mess -> break ("Attempt to insert object "
                     ++ "in " ++ toString parentSearchName 
                     ++ " which is not a package folder")
                  Right mmissPackageFolderLinkOpt ->
                     return mmissPackageFolderLinkOpt
               case mmissPackageFolderLinkOpt of
                  Nothing -> -- give up
                     return Nothing
                  Just mmissPackageFolderLink ->
                     do
                        mmissPackageFolder 
                           <- readLink view mmissPackageFolderLink
                        let
                           objectLoc = ObjectLoc {
                              package = mmissPackageFolder,
                              name = thisName,
                              ifExists = Nothing
                              }
                        return (Just objectLoc)
         Right (Just objectLink) ->
            do
               mmissObject <- readLink view objectLink

               packageFolderAndNameWE <- getMMiSSPackageFolderAndName
                  view (toLinkedObject mmissObject)

               (packageFolder,name) 
                     <- case fromWithError packageFolderAndNameWE of
                  Left mess -> break mess
                  Right success -> return success

               let
                  objectLoc = ObjectLoc {
                     package = packageFolder,
                     name = name,
                     ifExists = Just objectLink
                     }

               return (Just objectLoc)

-- | checkPreObjectTags checks that the objects which already exist in 
-- a PreObjects have
-- appropriate tags.
checkPreObjectTags :: View -> PreObjects -> IO (WithError ())
checkPreObjectTags view preObjects =
   do
      let
         objectsToBe :: [(ObjectLoc,[StructuredContent])]
         objectsToBe = listPreObjects preObjects

         listUnitWithError :: [WithError ()] -> WithError ()
         listUnitWithError errors 
            = mapWithError (const ()) (listWithError errors)

         checkObjectToBe :: (ObjectLoc,[StructuredContent]) 
            -> IO (WithError ())
         checkObjectToBe (ObjectLoc {ifExists = Nothing},contents) =
            return (hasValue ())
         checkObjectToBe (ObjectLoc {ifExists = Just link},contents) =
            do
               object <- readLink view link
               let
                  oldTag = xmlTag (mmissObjectType object)

                  checkContent :: StructuredContent -> WithError ()
                  checkContent structuredContent =
                     let
                        newTag = tag structuredContent
                     in
                        if oldTag == newTag
                           then
                              hasValue ()
                           else
                              hasError ("Object you define "
                                 ++ toString (label structuredContent) ++
                                "already exists, but with type " ++
                                oldTag ++ " rather than " ++ newTag)
                  errors :: [WithError ()]
                  errors = map checkContent contents

               return (listUnitWithError errors)

      (errors :: [WithError ()]) <- mapM checkObjectToBe objectsToBe

      return (listUnitWithError errors)

-- | Attempt to acquire the edit locks of all variants in the given 
-- PreObjects.  If successful, return an action for releasing those locks.
acquireEditLocks :: View -> PreObjects -> IO (WithError (IO ()))
acquireEditLocks view preObjects =
   do
      let
         preObjectsList :: [(ObjectLoc,[StructuredContent])]
         preObjectsList = listPreObjects preObjects

         getLock :: (ObjectLoc,[StructuredContent]) 
            -> IO (Maybe [(BSem,IO String)])
         getLock (ObjectLoc {ifExists = Nothing},_) = return Nothing
         getLock (ObjectLoc {ifExists = Just link},contents) =
            do
               object <- readLink view link
               let
                  messAct variants =
                     do
                        objectName <- getFullName view object
                        return (
                           "Unable to acquire edit lock for "
                           ++ objectName
                           ++ ": " ++ show variants
                           )

               (resultList :: [Maybe (BSem,IO String)])
                  <- mapM
                     (\ content ->
                        do
                           let
                              variants = variantSpec content

                           variableOpt <- lookupVariantObjectExact
                             (variantObject object)
                             variants
                           return (fmap
                              (\ variable ->
                                 (editLock variable,messAct variants))
                              variableOpt
                              )
                        )
                     contents
               return (Just (catMaybes resultList))

      (locks1 :: [Maybe [(BSem,IO String)]]) 
         <- mapM getLock preObjectsList

      let
         (locks2 :: [(BSem,IO String)]) = concat (catMaybes locks1)

      tryAcquireBSemsWithError fst snd locks2

---
-- newExtraNodes returns a suitable value for the extra nodes,
-- given the preamble link
newExtraNodes :: Link MMiSSPreamble 
   -> IO (Blocker (ArcData WrappedLink ArcType))
newExtraNodes mmissPreambleLink =
   do
      let
         setSource = staticSource [
            toArcData (WrappedLink mmissPreambleLink) preambleArcType True]
      newBlocker setSource

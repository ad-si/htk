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
import VariableSetBlocker
import Sources
import ReferenceCount

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

---
-- Creates or update an object from an Xml Element.
-- If it fails it returns a String.
-- The MMiSSObjectType is the expected type of the object.
--
-- If the bool checkThisEditLock is set we insist on getting the edit lock
-- of the top object, if it already exists.  In any case we get the edit lock
-- of all included objects.
--
-- The linked object supplied together with the element's label specify 
-- the object's new location.
-- Thus if the linked object is A and the label is B.C, the eventual location
-- will be A.B.C.  If an expectedLabel is supplied, this is checked against
-- the element's label, but it does not otherwise influence the function.
--
-- We support the following hack: if the Element's name is "." then
-- we write directly to the linked object itself.
writeToMMiSSObject :: Link MMiSSPreamble -> MMiSSObjectType -> View 
   -> LinkedObject -> Maybe EntityFullName -> Element -> Bool 
   -> IO (WithError (Link MMiSSObject))
writeToMMiSSObject preambleLink objectType view startLinkedObject 
   expectedLabel element checkThisEditLock =

   addFallOutWE (\ break ->
      (delay view) (do
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

         -- (3a) Get a link to the object, if it exists, and use it
         -- to construct a function which tells us if an ObjectLoc is the
         -- "Head" object.  We need this to identify the head object in
         -- the deconstructed PreObjects list.
         let
            -- (3a(i)) get the linked object for the containing dir of the
            -- object to be (which must exist).
            -- for this we use the following handy function, which will be
            -- reused.
            getContainingDir :: LinkedObject -> EntityFullName 
               -> IO LinkedObject
            getContainingDir linkedObject entityFullName =
               do
                  containingDirName <- case entityDir entityFullName of
                     Nothing -> break 
                        "Attempt to write to object with name \"\""
                     Just containingDirName -> return containingDirName
                   
                  containingDirOpt <- lookupFullNameInFolder linkedObject 
                     containingDirName

                  containingDir <- case containingDirOpt of
                     Nothing -> break ("Folder containing object "
                        ++toString entityFullName++" does not exist")
                     Just containingDir -> return containingDir

                  return containingDir


         -- Function to tell us if this is the head object.
         (isThisObject :: ObjectLoc -> Bool) <- 
            case entityBase objectLabel of
               Nothing -> 
                  let
                     isThisObject (NewObject _ _) = False
                     isThisObject (OldObject _ object) =
                        startLinkedObject == toLinkedObject object
                  in
                     return isThisObject
               Just objectLabelBase ->
                  do
                     containingDir 
                        <- getContainingDir startLinkedObject objectLabel
                     linkedObjectOpt <- lookupNameSimple containingDir
                        (toString objectLabelBase)
                     let
                        isThisObject :: ObjectLoc -> Bool
                        isThisObject objectLoc = 
                           case (linkedObjectOpt,objectLoc) of
                              (Just linkedObject,OldObject link2 _) -> 
                                 (toWrappedLink linkedObject) 
                                 == (WrappedLink link2)
                              (Nothing,NewObject containingDir2 entityName) ->
                                 entityName == objectLabelBase 
                                    && containingDir2 == containingDir
                              _ -> False
                     return isThisObject

         -- (4) Construct a PreObjects for all the constituent objects.

         -- We also (since it is now easy to do) check that the objects
         -- being written to have the right XML tag.
         let
            -- We use treeFoldM.  In its type,
            -- ancestorInfo is the (LinkedObject,EntityPath) with respect to
            -- which we look up the object, using lookupObjectByPath.
            --   (N.B. That means that for a path belonging to an object,
            --   the path needs to be raised if the corresponding object
            --   is passed, since for example the path "." attached to an 
            --   MMiSSObject means "Start from the *parent* of the object".
            -- state is the above list
            -- node is the structured contents.

            -- So we need the following function.
            treeFoldFn :: 
               (LinkedObject,EntityPath) -> PreObjects -> StructuredContent
               -> IO ((LinkedObject,EntityPath),PreObjects,
                  [StructuredContent])
            treeFoldFn (linkedObject,entityPath) preObjects0
               structuredContent =
               do
                  let
                     name = label structuredContent
                  -- We need to find the object.  Our strategy is as follows
                  -- (1) we attempt to find the object.
                  -- (2) if it doesn't exist, we look for the containing 
                  --     folder (where we will have to construct the object).
                  (objectLinkOptWE :: WithError (Maybe (Link MMiSSObject)))
                     <- lookupObjectByPath linkedObject entityPath name
                  let
                     objectLinkOpt = coerceWithErrorOrBreak break 
                        objectLinkOptWE

                     thisPath = path structuredContent
 
                  -- Obtain the first element of PreObject, and an object and
                  -- path to look up the children.
                  --
                  -- If the object does not yet exist we use the parent.
                  (objectLoc,linkedObject,path) <- case objectLinkOpt of
                     Nothing ->
                        do
                           let
                              baseNameOpt = entityBase name
                              baseName = fromMaybe 
                                 (break "MMiSSObjects bug 2") baseNameOpt

                           containingDir <- getContainingDir linkedObject name

                           let
                              objectLoc = NewObject containingDir baseName

                           return (objectLoc,containingDir,thisPath)
                     Just objectLink ->
                        do
                           object <- readLink view objectLink

                           let
                              oldTag = xmlTag (mmissObjectType object)
                              newTag = tag structuredContent
                           unless (oldTag == newTag)
                              (break ("Object you define "++toString name++
                                "already exists, but with type "++
                                oldTag++" rather than "++newTag))
   
                           let
                              objectLoc = OldObject objectLink object 
                           return (objectLoc,
                              toLinkedObject object,raiseEntityPath thisPath)

                  preObjects1 <- coerceWithErrorOrBreakIO break (
                     addObject objectLoc structuredContent preObjects0)

                  return ((linkedObject,path),preObjects1,
                     childs structuredContent)

         -- Now run treeFoldM.
         (preObjects :: PreObjects) <- treeFoldM treeFoldFn 
            (startLinkedObject,trivialPath)
            emptyPreObjects contents

         -- (5) Get list of objects-to-be.
         let
            preObjectsList0 :: [(ObjectLoc,[StructuredContent])]
            preObjectsList0 = listPreObjects preObjects

            -- Rearrange it to extract the "head" object.
            splitOpt = splitToElemGeneral 
               (\ (objectLoc,_) ->
                  isThisObject objectLoc
                  )
               preObjectsList0
            
            (preHead,headObject,postHead) = case splitOpt of
               Just split -> split
               Nothing -> break "MMiSSWriteObject bug; no head object found!"
      
            -- The list, without the head object,
            preObjectsList1 = preHead ++ postHead

            -- The list, with the head object first.
            preObjectsList2 = headObject:preObjectsList1
 
         -- (6) Attempt to grab all the editLocks for all variants which are
         --     already in existence, unless this one if checkThisEditLock
         --     is set.
         let
            preObjectsToLock =
              if checkThisEditLock then preObjectsList2 else preObjectsList1

         (bSems0 :: [[Maybe (BSem,String)]]) <-
            mapM
               (\ (objectLoc,contentsList) -> case objectLoc of
                  NewObject _ _ -> return []
                  OldObject _ object ->
                     do
                        -- We use unsafeInterleaveIO to prevent the expensive
                        -- computation of the name until the
                        -- String is needed.
                        name <- unsafeInterleaveIO (objectName object)
                        mapM
                           (\ contents -> 
                              do
                                 variableOpt <- lookupVariantObjectExact 
                                    (variantObject object) 
                                    (variantSpec contents)

                                 return (fmap
                                    (\ variable -> 
                                         (editLock variable,name))
                                    variableOpt
                                    )
                              )
                           contentsList
                  )
               preObjectsToLock

         let
            bSems1 :: [(BSem,String)] 
            bSems1 = concat (map catMaybes bSems0)

         releaseActWE <- tryAcquireBSemsWithError fst snd bSems1

         releaseAct <- coerceWithErrorOrBreakIO break releaseActWE

         -- (7) Add all objects, return links to them with the head object 
         --     first.
         newLinks <- 
            synchronizeView view (
               Control.Exception.finally (
                  mapM (simpleWriteToMMiSSObject preambleLink view break) 
                     preObjectsList2
                     )
                  releaseAct
                  )

         -- (8) return a link to the head object
         return (head newLinks)
      ))


---
-- Create a single object.  The contents list must be non-empty.
simpleWriteToMMiSSObject :: Link MMiSSPreamble -> View -> BreakFn 
   -> (ObjectLoc,[StructuredContent])
   -> IO (Link MMiSSObject)
simpleWriteToMMiSSObject preambleLink view break (objectLoc,contentsList) =
   do
      -- (1) Construct the object, if necessary.
      -- This does not actually insert the object in the folder, the reason
      -- being that we want to hide the object from inspection by the display
      -- routine until something has been put in its variant dictionary.
      -- Instead we return "afterAct", which does precisely that.
      (objectLink,object,afterAct :: IO (WithError ())) <- case objectLoc of
         OldObject objectLink object -> return (objectLink,object,
            return (hasValue ()))
         NewObject parentLinkedObject entityName ->
            do
               let
                  mmissObjectType 
                     = retrieveObjectType (tag (head contentsList))

               nodeActions <- newNodeActionSource
               extraNodes <- newExtraNodes preambleLink

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
               return (objectLink,object,afterAct)


      let

         -- (2) Insert the variants in the object
         ---
         -- Split the contents.  contents1 is distinguished because we make it
         -- the current variant, if it doesn't already appear.  (Yes, this is
         -- somewhat random.)
         contents1 : contentsRest = contentsList

         varObject = variantObject object

         ---
         -- Function for inserting a contents item.  The Bool indicates whether
         -- we make it the current item or not.
         insertItem :: Bool -> StructuredContent -> IO ()
         insertItem doPoint content =
            do
               let
                  element = Elem (tag content)
                     (attributes content)
                     (contents (accContents content))

               oldVariableOpt <- lookupVariantObjectExact varObject
                  (variantSpec content)
               case oldVariableOpt of
                  Just (variable @ Variable {element = elementLink}) ->
                     -- The only thing we do is write the new element to
                     -- the link.
                     writeLink view elementLink element
                  Nothing ->
                     do
                        -- Create the new object
                        elementLink <- createLink view element
                        editLock <- newBSem
                        let
                           variable = Variable {
                              element = elementLink,
                              preamble = preambleLink,
                              editLock = editLock
                              }

                           write = if doPoint then writeVariantObjectAndPoint 
                              else writeVariantObject

                        write varObject (variantSpec content) variable

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

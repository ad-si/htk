module LinkManager(
   -- Classes
   HasLinkedObject(..),
      -- those things which have a LinkedObject.  (No instances are
      -- declared in this file.)
   -- Types
   LinkedObject, 
      -- This corresponds to some object in the repository.
      -- Instance of HasCodedValue, Eq, Ord.
   LinkSource,
      -- This represents a set of links (EntitySearchName's) to search for.
      -- We also carry (with the LinkSource) values of some type specified
      -- as a parameter to LinkSource.
   FrozenLinkSource,
      -- a way of storing a LinkSource.  Instance of HasCodedValue.
   

   Insertion,
      -- This represents somewhere to put a LinkedObject.

   -- LinkedObject Functions
   newLinkedObject, 
      -- :: View -> WrappedLink -> Maybe Insertion 
      -- -> IO (WithError LinkedObject)
      -- Create a new LinkedObject.

      -- Each LinkedObject contains a WrappedLink which is assumed to point
      -- to the object containing the LinkedObject.  In particular, when the
      -- linked object is changed, we dirty the WrappedLink.
   newLinkedPackageObject,
      -- :: View -> WrappedLink -> Maybe Insertion 
      -- ->IO (WithError LinkedObject)
      -- Similar to newLinkedObject, but for a LinkedObject which will
      -- represent a package object, for example an MMiSS package folder.
      -- A newLinkedPackageObject always starts off with trivialImportCommands,
      -- whether after newLinkedPackageObject or decoding or merging.  
      -- The ImportCommands must be set by setCommands.

   moveObject,
      -- :: LinkedObject -> Maybe Insertion -> IO (WithError ())
      -- Change the location of a LinkedObject
      -- Nothing means take the LinkedObject out of its parent and not
      -- put it anywhere else.
   mkInsertion,
      -- :: LinkedObject -> EntityName -> Insertion
      -- Make an insertion
   unmkInsertion,
      -- :: Insertion -> (LinkedObject,EntityName)
      -- Inverse of mkInsertion.
   getCurrentInsertion,
      -- :: LinkedObject -> IO (Maybe Insertion)

   toInsertion,
      -- :: LinkedObject -> SimpleSource (Maybe Insertion)
      -- Get the insertions for an object.

   deleteLinkedObject,
      -- :: View -> LinkedObject -> IO ()
      -- Delete an object including its record in the view and the parent
      -- folder and anywhere else (currently nowhere) where the Link is
      -- stored by the LinkManager, EXCEPT in LinkEnvironments, which should
      -- be disposed of by the caller.

   createLinkedObjectChild,
      -- :: ObjectType objectType object
      -- => View -> LinkedObject -> EntityName -> (LinkedObject -> IO object) 
      -- -> IO (Maybe (Link object))
      -- Create a new linked object, to be the child of the given linked 
      -- object.  We return a link to the new object.  If there is an error
      -- we display the message. 

   createLinkedObjectChildSplit,
      -- :: ObjectType objectType object
      -- => View -> LinkedObject -> EntityName -> (LinkedObject -> IO object) 
      -- -> IO (Maybe (Link object,IO (WithError ())))
      -- Create a new linked object, to be the child of the given linked 
      -- object.  We return a link to the new object.  However we do not 
      -- actually do the insertion, instead we return an action which does
      -- the insertion (or, possibly, fails).
  
   objectContents,
      -- :: LinkedObject -> VariableSetSource WrappedLink
      -- Get the contents of a LinkedObject (those objects contained in it,
      -- like elements in a folder)

   listObjectContents,
      -- :: LinkedObject -> SimpleSource [(EntityName,LinkedObject)]
      -- List the contents of an object by EntityName.

   lookupObjectContents,
      -- :: LinkedObject -> EntityName -> SimpleSource (Maybe WrappedLink)
      -- Get a SimpleSource item corresponding to the EntityName element of
      -- the object contents.

   lookupNameSimple,
      -- :: LinkedObject -> String -> IO (Maybe LinkedObject)
      -- Extract a single element in a linkedObject's contents by name.

   lookupNameInFolder,
      -- :: LinkedObject -> EntityName -> IO (Maybe LinkedObject)
      -- Extract a single element in a linkedObject's contents by EntityName
   lookupObjectInFolder,
      -- :: LinkedObject -> EntityName -> IO (Maybe (Link object))
      -- Extract an object in a linkedObject's contents by EntityName
   lookupFullNameInFolder,
      -- :: LinkedObject -> EntityFullName -> IO (Maybe LinkedObject)
      -- Extract a full name as a sub object of a given object.
   lookupLinkedObjectByFullName, 
      -- :: View -> EntityFullName -> IO (Maybe LinkedObject)
      -- Look up an object in a view by full name.
      -- We return Nothing if the object does not exist.
 
   toWrappedLink,
      -- :: LinkedObject -> WrappedLink
      -- Get the WrappedLink in a LinkedObject.
   getLinkedObjectTitle,
      -- :: LinkedObject -> EntityName -> SimpleSource EntityName
      -- Extract the EntityName from the insertion in a LinkedObject.
      -- The second argument is a default String to use, if the Insertion is
      -- Nothing.
   getLinkedObjectTitleOpt,
      -- :: LinkedObject -> SimpleSource (Maybe EntityName)
      -- Extract the EntityName from a LinkedObject, if any.


   toObjectLink, 
      -- :: (HasLinkedObject object,ObjectType objectType object)
      -- => object -> Link object
      -- Get an object's Link, assuming that it's stored in its WrappedLink.
   toParentLink,
      -- :: (HasLinkedObject object1,ObjectType objectType2 object2)
      -- => object1 -> IO (WithError (Maybe (Link object2)))
      -- Extract the current parent of an object (as a Link object2) if it
      -- exists, otherwise return Nothing.
      --
      -- If the parent exists but has the wrong type we return an error, via
      -- WithError.

   -- Functions which can be used only on LinkedObject's created by
   -- newLinkedPackageObject.
   setCommands,
      -- :: LinkedObject -> ImportCommands -> IO ()
      -- Set the ImportCommands.  If this linked object was not created
      -- by newLinkedPackageObject, the result will be an error.
   lookupObject,
      -- :: ObjectType objectType object 
      -- => View -> LinkedObject -> EntitySearchName 
      -- -> IO (WithError (Maybe (Link object)))
      -- Looks up an object, returning the actual link if possible.
      -- We return Nothing if the object does not exist but cause an error if 
      -- the object has the wrong type.
   lookupLinkedObject,
      -- :: View -> LinkedObject -> EntitySearchName 
      -- -> IO (WithError (Maybe LinkedObject))
      -- :: ObjectType objectType object 
      -- => View -> LinkedObject -> EntitySearchName 
      -- -> IO (Maybe LinkedObject)
      -- Like lookupObject, but instead returns the LinkedObject.


   getFullName, 
       -- :: HasLinkedObject object => View -> object -> IO String
       -- Returns the full name of an object within the view.
       -- Used for error messages and packageIds.

   bracketForImportErrors,
      -- :: View -> IO a -> IO a
      -- Wrap around a user action which changes the topology of the links,
      -- to catch and report potential import errors.

   -- FolderStructure functions.
   toFolderStructure,
      -- :: LinkedObject -> FolderStructure LinkedObject
      -- The first argument is the root.

   -- LinkSource functions
   newLinkSource, 
      -- :: View -> LinkedObject -> [(EntityFullName,value)] 
      -- -> IO (LinkSource value)
      -- Create a new LinkSource.  The LinkedObject must be one
      -- created by newLinkedPackageObject.
   setLinks,
      -- :: LinkSource value -> [(EntitySearchName,value)] -> IO ()
      -- Set the links for the LinkSource.
   listFromLinkSource,
      -- :: LinkSource value -> SimpleSource [(LinkedObject,value)]
      -- Obtain the pointed-to elements for a LinkSource.

   freezeLinkSource,
      -- :: LinkSource value -> IO (FrozenLinkSource value)
      -- Freeze a LinkSource (used on encoding).

   createLinkSource,
      -- :: Linked -> FrozenLinkSource value -> IO (LinkSource value)
      -- Create a LinkSource given a newLinkedPackageObject object and a
      -- FrozenLinkSource (used on decoding)

   mkArcEnds,
      -- :: Bool -> LinkSource value -> (value -> ArcType) -> ArcEnds
      -- Construct the ArcEnds for a linkSource (with their paired
      -- ArcType's).  The Bool is taken as the mustFollow value.  
   mkArcEndsSource,
      -- :: LinkSource value -> (value -> ArcType) ->
      -- SimpleSource [(WrappedLink,ArcType)]
      -- Construct a source containing the out-arcs from a LinkSource.


   -- The following functions are used during merging.

   getLinkedObjectMergeLinks,
      -- :: (HasCodedValue object,HasLinkedObject object) 
      -- => MergeLinks object
      -- Suitable MergeLinks for a LinkedObject.

   attemptLinkedObjectMerge,
      -- :: ObjectType objectType object
      -- => MergeTypes.LinkReAssigner -> View -> 
      -- Link object -> [(View,LinkedObject)] -> IO (WithError LinkedObject)
      -- Merge several versions of a LinkedObject.  To be used in conjunction
      -- with getLinkedObjectMergeLinks.

   linkedObjectsSame,
      -- :: LinkedObject -> LinkedObject -> IO Bool
      -- Returns true if the CodedValue representations of the
      -- two LinkedObject's are identical.  (The LinkedObject's are 
      -- likely enough in different views.)
   ) where

import Maybe

import IOExts
import Control.Concurrent.MVar
import Data.FiniteMap

import Debug
import Computation
import ExtendedPrelude
import Sources
import Broadcaster
import Sink
import Dynamics
import VariableSet
import VariableList
import AtomString(fromStringWE,toString)
import Object
import Messages

import DialogWin

import FolderStructure
import Imports

import LinkDrawer
import CodedValue
import ObjectTypes
import EntityNames
import Link
import ViewType
import View
import MergeTypes
import {-# SOURCE #-} Folders

-- ----------------------------------------------------------------------
-- User interface
-- ----------------------------------------------------------------------

-- those things which have a LinkedObject.
class HasLinkedObject object where
   toLinkedObject :: object -> LinkedObject

instance HasLinkedObject LinkedObject where
   toLinkedObject linkedObject = linkedObject

---
-- Create a new LinkedObject.
newLinkedObject :: View -> WrappedLink -> Maybe Insertion 
   -> IO (WithError LinkedObject)
newLinkedObject view wrappedLink insertionOpt =
   do
      let
         frozenLinkedObject = FrozenLinkedObject {
            wrappedLink' = wrappedLink,
            insertion' = insertionOpt,
            contents' = [],
            hasImportCommands = False
            }
      createLinkedObject view True frozenLinkedObject

newLinkedPackageObject :: View -> WrappedLink -> Maybe Insertion 
   -> IO (WithError LinkedObject)
newLinkedPackageObject view wrappedLink insertionOpt =
   do
      let
         frozenLinkedObject = FrozenLinkedObject {
            wrappedLink' = wrappedLink,
            insertion' = insertionOpt,
            contents' = [],
            hasImportCommands = True
            }
      createLinkedObject view True frozenLinkedObject

---
-- Get the contents of a LinkedObject (those objects contained in it,
-- like elements in a folder).
objectContents :: LinkedObject -> VariableSetSource WrappedLink
objectContents linkedObject =
   listToSetSource
      (fmap
         (\ fm ->
            map 
               (\ linkedObjectPtr -> wrappedLinkInPtr linkedObjectPtr)
               (eltsFM fm)
            )
         (toSimpleSource (contents linkedObject))
         )


-- | List the contents of an object by EntityName.
listObjectContents :: LinkedObject -> SimpleSource [(EntityName,LinkedObject)]
listObjectContents linkedObject =
   fmap
      (\ fm -> 
         map
            (\ (entityName,linkedObjectPtr) 
               -> (entityName,fromLinkedObjectPtr linkedObjectPtr))
            (fmToList fm)
         )
      (toSimpleSource (contents linkedObject))

--
-- Get a SimpleSource item corresponding to the EntityName element of
-- the object contents.
lookupObjectContents :: LinkedObject -> EntityName 
   -> SimpleSource (Maybe WrappedLink)
lookupObjectContents linkedObject entityName =
   fmap
      (fmap wrappedLinkInPtr)
      (lookupEntityName linkedObject entityName)

---
-- Delete an object including its record in the view and the parent
-- folder and anywhere else (currently nowhere) where the Link is
-- stored by the LinkManager. 
deleteLinkedObject :: View -> LinkedObject -> IO ()
deleteLinkedObject view linkedObject =
   do
      moveObject linkedObject Nothing
      deleteWrappedLink (toWrappedLink linkedObject)
   where
      deleteWrappedLink :: WrappedLink -> IO ()
      deleteWrappedLink (WrappedLink link) = deleteLink view link

---
-- Create a new linked object, to be the child of the given linked 
-- object.  We return a link to the new object.  If there is an error
-- we display the message. 
createLinkedObjectChild :: ObjectType objectType object
   => View -> LinkedObject -> EntityName -> (LinkedObject -> IO object) 
   -> IO (Maybe (Link object))
createLinkedObjectChild view parentLinkedObject name getObject =
   do
      objectDataOpt <- createLinkedObjectChildSplit view parentLinkedObject 
         name getObject
      case objectDataOpt of
         Nothing -> return Nothing
         Just (objectLink,insertionAct) ->
            do
               insertionWE <- insertionAct
               case (fromWithError insertionWE) of
                  Right () -> return (Just objectLink)
                  Left mess ->
                     do
                        errorMess mess
                        return Nothing

---
-- Create a new linked object, to be the child of the given linked 
-- object.  We return a link to the new object.  However we do not 
-- actually do the insertion, instead we return an action which does
-- the insertion (or, possibly, fails).
createLinkedObjectChildSplit :: ObjectType objectType object
   => View -> LinkedObject -> EntityName -> (LinkedObject -> IO object) 
   -> IO (Maybe (Link object,IO (WithError ())))
createLinkedObjectChildSplit view parentLinkedObject name getObject =
   do
      let
         insertFolderAct linkedObject1 =
            do
               let
                  insertion = mkInsertion parentLinkedObject name
               moveObject linkedObject1 (Just insertion)

      act <- createViewObject view (\ link ->
         do
            linkedObjectWE <- newLinkedObject
               view (WrappedLink link) Nothing
            case fromWithError linkedObjectWE of
               Right linkedObject ->
                  do
                     object <- getObject linkedObject
                     return (Just object,
                        return (Just (link,insertFolderAct linkedObject)))
               Left mess ->
                  return (Nothing,
                     do
                        errorMess mess
                        return Nothing
                     )
         )
      act

setCommands :: LinkedObject -> ImportCommands -> IO ()
setCommands linkedObject importCommands1 =
   case importCommands linkedObject of
      Nothing -> 
         putStrLn ("LinkManager.setCommands bug  "
            ++ "-- attempt to set import commands for non-package object")
      Just broadcaster -> broadcast broadcaster importCommands1
         

---
-- Extract a single element in a linkedObject's contents by name.
lookupNameSimple :: LinkedObject -> String -> IO (Maybe LinkedObject)
lookupNameSimple linkedObject str =
   do
      let
         entityNameWE = fromStringWE str
      case fromWithError entityNameWE of
         Left _ -> return Nothing
         Right entityName -> lookupNameInFolder linkedObject entityName


-- | Extract an object in a linkedObject's contents by EntityName
lookupObjectInFolder :: ObjectType objectType object
   => LinkedObject -> EntityName -> IO (Maybe (Link object))
lookupObjectInFolder linkedObject entityName =
   do
      linkedObjectOpt <- lookupNameInFolder linkedObject entityName
      return (case linkedObjectOpt of
         Nothing -> Nothing
         Just linkedObject ->
            let
               wrappedLink = toWrappedLink linkedObject
            in
               unpackWrappedLink wrappedLink
         )

-- | Extract a single element in a linkedObject's contents by EntityName
lookupNameInFolder :: LinkedObject -> EntityName -> IO (Maybe LinkedObject)
lookupNameInFolder linkedObject entityName =
   do
      linkedObjectPtrOpt <- readContents (
         lookupEntityName linkedObject entityName)
      return (fmap fromLinkedObjectPtr linkedObjectPtrOpt)

---
-- Extract a full name as a sub object of a given object.
lookupFullNameInFolder :: LinkedObject -> EntityFullName 
   -> IO (Maybe LinkedObject)
lookupFullNameInFolder linkedObject (EntityFullName names) 
      = lookup1 linkedObject names
   where
      lookup1 :: LinkedObject -> [EntityName] -> IO (Maybe LinkedObject)
      lookup1 linkedObject [] = return (Just linkedObject) 
      lookup1 linkedObject (name : names) =
         do
            linkedObjectPtrOpt 
               <- readContents (lookupEntityName linkedObject name)
            case linkedObjectPtrOpt of
               Nothing -> return Nothing
               Just linkedObjectPtr 
                  -> lookup1 (fromLinkedObjectPtr linkedObjectPtr) names

---
-- Make an insertion
mkInsertion :: LinkedObject -> EntityName -> Insertion
mkInsertion linkedObject entityName =
   Insertion {
      parent = thisPtr linkedObject,
      name = entityName
      }

---
-- Get the contents of an insertion
unmkInsertion :: Insertion -> (LinkedObject,EntityName)
unmkInsertion (Insertion {parent = parent,name = name}) =
   (fromLinkedObjectPtr parent,name)
      

-- Get the insertions for an object.
toInsertion :: LinkedObject -> SimpleSource (Maybe Insertion)
toInsertion = insertion

getCurrentInsertion :: LinkedObject -> IO (Maybe Insertion)
getCurrentInsertion linkedObject = readContents (insertion linkedObject)

---
-- Get the WrappedLink in a LinkedObject.
toWrappedLink :: LinkedObject -> WrappedLink
toWrappedLink linkedObject = wrappedLinkInPtr (thisPtr linkedObject)

---
-- Get an object's Link, assuming that it's stored in its WrappedLink.
toObjectLink :: (HasLinkedObject object,ObjectType objectType object)
   => object -> Link object
toObjectLink object =
   let
      linkedObject = toLinkedObject object
      wrappedLink = toWrappedLink linkedObject
      linkOpt = unpackWrappedLink wrappedLink
   in
      fromMaybe
         (error ("LinkManager.toObjectLink: object's WrappedLink does not "++
                "contain a link to it"))
         linkOpt

---
-- Extract the current parent of an object (as a Link object2) if it
-- exists, otherwise return Nothing.
--
-- If the parent exists but has the wrong type we return an error, via
-- WithError.
toParentLink :: (HasLinkedObject object1,ObjectType objectType2 object2)
   => object1 -> IO (WithError (Maybe (Link object2)))
toParentLink object1 =
   do
      let
         linkedObject1 = toLinkedObject object1
      insertionOpt <- readContents (insertion linkedObject1)
      return (case insertionOpt of
         Nothing -> hasValue Nothing
         Just (Insertion {parent = parent}) ->
            case unpackWrappedLink (wrappedLinkInPtr parent) of
               Nothing -> hasError 
                  "Object has parent, but parent is of the wrong type"
               Just link -> hasValue (Just link)
         )

---
-- Extract the EntityName from the insertion in a LinkedObject.
-- The second argument is a default String to use, if the Insertion is
-- Nothing.
getLinkedObjectTitle :: LinkedObject -> EntityName -> SimpleSource EntityName
getLinkedObjectTitle linkedObject def =
   fmap (fromMaybe def) (getLinkedObjectTitleOpt linkedObject)

---
-- Extract the EntityName from a LinkedObject, if any.
getLinkedObjectTitleOpt :: LinkedObject -> SimpleSource (Maybe EntityName)
getLinkedObjectTitleOpt linkedObject =
   fmap (fmap name) (insertion linkedObject)

-- | Look up an object in a view by full name.
-- We return Nothing if the object does not exist.
lookupLinkedObjectByFullName :: View -> EntityFullName 
   -> IO (Maybe LinkedObject)
lookupLinkedObjectByFullName view fullName =
   do
      importsState <- getImportsState view
      let
         folders0 = folders importsState
      source <- lookupFullName folders0 (root folders0) fullName
      readContents source

---
-- Looks up an object, returning the actual link if possible.
-- We return Nothing if the object does not exist but cause an error if the
-- object has the wrong type.
lookupObject :: ObjectType objectType object 
   => View -> LinkedObject -> EntitySearchName 
   -> IO (WithError (Maybe (Link object)))
lookupObject view linkedObject searchName =
   do
      linkedObjectOptWE <- lookupLinkedObject view linkedObject searchName
      return (mapWithError'
         (\ linkedObjectOpt ->
            case linkedObjectOpt of
               Nothing -> return Nothing
               Just linkedObject ->
                  let
                     wrappedLink = wrappedLinkInPtr (thisPtr linkedObject)
                     linkWE = unpackWrappedLinkWE wrappedLink
                  in
                     case fromWithError linkWE of
                        Left mess -> hasError (toString searchName
                           ++ ": " ++ mess)
                        Right link -> hasValue (Just link)
            )
         linkedObjectOptWE
         )

-- | Like lookupObject, but instead returns the LinkedObject.
lookupLinkedObject :: View -> LinkedObject -> EntitySearchName 
   -> IO (WithError (Maybe LinkedObject))
lookupLinkedObject view linkedObject searchName =
   do
      importsState <- getImportsState view
      bracketForImportErrors2 importsState (
         do
            source <- lookupNode importsState linkedObject searchName
            lookupResult <- readContents source 
            return (case lookupResult of
               NotFound -> hasValue Nothing
               Error -> hasError (toString searchName 
                  ++ " cannot be found because of errors") 
               Found linkedObject -> hasValue (Just linkedObject)
               )
         )


-- | Returns the full name of an object within the view.
-- Used for error messages and packageIds.
getFullName :: HasLinkedObject object => View -> object -> IO String
getFullName view object =
   do
      importsState <- getImportsState view
      fullName <- getName (folders importsState) (toLinkedObject object)
      return (toString fullName) 

-- | Wrap around a user action which changes the topology of the links,
-- to catch and report potential import errors.
bracketForImportErrors :: View -> IO a -> IO a
bracketForImportErrors view act =
   do
      importsState <- getImportsState view
      bracketForImportErrors2 importsState act


---
-- (function not used any longer)
unpackLinkedObjectPtr :: ObjectType objectType object 
   => BreakFn -> Maybe LinkedObjectPtr -> IO (Maybe (Link object))
unpackLinkedObjectPtr break Nothing = return Nothing
unpackLinkedObjectPtr break (Just linkedObjectPtr) = 
   do
      let
         wrappedLink = wrappedLinkInPtr linkedObjectPtr
         linkOpt = unpackWrappedLink wrappedLink
      
      link <- case linkOpt of
         Just link -> return link
         Nothing ->
            break ("Object exists, but with the wrong type")

      seq link (return (Just link))

---
-- Create a new LinkSource
newLinkSource :: View -> LinkedObject -> [(EntitySearchName,value)] 
  -> IO (LinkSource value)
newLinkSource = createLinkSourceGeneral

---
-- Obtain the pointed-to elements for a LinkSource.
listFromLinkSource :: LinkSource value -> SimpleSource [(LinkedObject,value)]
listFromLinkSource linkSource = 
   fmap catMaybes (targetsSource linkSource)

-- | Construct a source containing the out-arcs from a LinkSource.
mkArcEndsSource :: LinkSource value -> (value -> ArcType) ->
   SimpleSource [(WrappedLink,ArcType)]
mkArcEndsSource (linkSource :: LinkSource value) toArcType =
   let
      arcEnds0 :: SimpleSource [(LinkedObject,value)]
      arcEnds0 = listFromLinkSource linkSource

      arcEnds1 :: SimpleSource [(WrappedLink,ArcType)]
      arcEnds1 =
         fmap
            (map (\ (link,value) -> (toWrappedLink link,toArcType value)))
            arcEnds0
   in
      arcEnds1


-- | Construct the ArcEnds for a linkSource (with their paired
-- ArcType's).  The Bool is taken as the mustFollow value.  
mkArcEnds :: Bool -> LinkSource value -> (value -> ArcType) -> ArcEnds
mkArcEnds mustFollow (linkSource :: LinkSource value) toArcType =
   let
      arcEnds1 = mkArcEndsSource linkSource toArcType

      arcEnds2 :: VariableList (WrappedLink,ArcType)
      arcEnds2 = newVariableListFromList arcEnds1

      arcEnds3 :: VariableList (ArcData WrappedLink ArcType)
      arcEnds3 = fmap
         (\ (wrappedLink,arcType) ->  
            toArcData wrappedLink arcType mustFollow
            )
         arcEnds2

   in
      arcEnds3

instance Eq LinkedObject where
   (==) obj1 obj2 = (thisPtr obj1) == (thisPtr obj2)

instance Ord LinkedObject where
   compare obj1 obj2 = compare (thisPtr obj1) (thisPtr obj2)

-- ----------------------------------------------------------------------
-- The user-visible types
--
-- LinkedObject, LinkSource are instances of HasCodedValue.
-- ----------------------------------------------------------------------

data LinkedObject = LinkedObject {
   thisPtr :: LinkedObjectPtr,
   insertion :: SimpleSource (Maybe Insertion),
   contents :: SimpleBroadcaster (FiniteMap EntityName LinkedObjectPtr),
   moveObject :: Maybe Insertion -> IO (WithError ()),
   importCommands :: Maybe (SimpleBroadcaster ImportCommands),

   contentsSource :: SimpleSource (FiniteMap EntityName LinkedObjectPtr),
   importCommandsSource ::Maybe (SimpleSource ImportCommands)


   }


data Insertion = Insertion {
   parent :: LinkedObjectPtr, -- folder to contain this object
   name :: EntityName -- name it shall have
   } deriving (Eq)

data LinkSource value = LinkSource {
   object :: LinkedObject,
   linksSource :: SimpleSource [(EntitySearchName,value)],
   targetsSource :: SimpleSource [Maybe (LinkedObject,value)],
   setLinks :: [(EntitySearchName,value)] -> IO ()
   }

-- ----------------------------------------------------------------------
-- Defining the FolderStructure
-- ----------------------------------------------------------------------

toFolderStructure :: LinkedObject -> FolderStructure LinkedObject
toFolderStructure root =
   let
      getContentsSource linkedObject =
         return .
            (fmap
               (mapFM (\ _ -> fromLinkedObjectPtr))
               )
            . contentsSource $ linkedObject

      getImportCommands linkedObject = 
         return . importCommandsSource $ linkedObject

      getParent linkedObject =
         return 
         . (fmap 
            (fmap 
               (\ insert -> 
                  (fromLinkedObjectPtr . parent $ insert,name insert)
                  )
               )
            ) 
         . insertion $ linkedObject
   in
      FolderStructure {
         root = root,
         getContentsSource = getContentsSource,
         getImportCommands = getImportCommands,
         getParent = getParent
         }         

-- ----------------------------------------------------------------------
-- Freezing and defrosting/creating. a LinkedObject
-- ----------------------------------------------------------------------

freezeLinkedObject :: LinkedObject -> IO FrozenLinkedObject
freezeLinkedObject linkedObject =
   do
      let
         wrappedLink' = wrappedLinkInPtr (thisPtr linkedObject)
      insertion' <- readContents (insertion linkedObject)
      contentsData <- readContents (contents linkedObject)
      let
         (contents' :: [(EntityName,LinkedObjectPtr)]) = fmToList contentsData
         hasImportCommands = isJust (importCommands linkedObject)

      return (FrozenLinkedObject {wrappedLink' = wrappedLink',
         insertion' = insertion',contents' = contents',
         hasImportCommands = hasImportCommands})

---
-- Create a new linked object, given the FrozenLinkedObject.
-- This can occur in two cases (1) when the linkedObject is absolutely new;
-- (2) when it is being decodeIO'd from the repository or created by a merge. 
-- The Bool indicates
-- which of these applies; if True it means the linkedObject is absolutely new.
-- In that case, we also attempt to insert it into the parent folder (if
-- any).  
--
-- Renamings are synchronized on the view, which should mean that
-- commits can't happen half-way through.
createLinkedObject :: View -> Bool -> FrozenLinkedObject
    -> IO (WithError LinkedObject)
createLinkedObject view isNew frozenLinkedObject =
   do
      let
         wrappedLink = wrappedLink' frozenLinkedObject
         insertion0 = insertion' frozenLinkedObject

      insertionBroadcaster <- newSimpleBroadcaster insertion0

      contents1 
         <- newSimpleBroadcaster (listToFM (contents' frozenLinkedObject))

      importCommands <-
         if hasImportCommands frozenLinkedObject
            then
               do
                  broadcaster <- newSimpleBroadcaster trivialImportCommands
                  return (Just broadcaster)
            else
               return Nothing      

      -- Create mirrored delayed versions of sources (for the FolderStructure
      -- we will create).

      (insertion,_) <- mirrorSimpleSourceWithDelayer (delayer view)
         (toSimpleSource insertionBroadcaster)

      (contentsSource,_) <- mirrorSimpleSourceWithDelayer (delayer view) 
         (toSimpleSource contents1)

      importCommandsSource <- case importCommands of
         Nothing -> return Nothing
         Just importCommands1 ->
            do
               (importCommandsSource1,_) <- mirrorSimpleSourceWithDelayer
                  (delayer view) (toSimpleSource importCommands1)
               return (Just importCommandsSource1)
      -- previousMVar contains the last insertion (or initially Nothing);
      -- it is also used as a lock to prevent moveObject being used 
      -- twice simultaneously on the same LinkedObject.
      previousMVar <- newMVar (if isNew then Nothing else insertion0)

      let
         moveObject insertion = synchronizeView view (
            do
               previous <- takeMVar previousMVar
               let
                  toContents insertion 
                     = contents (fromLinkedObjectPtr (parent insertion))

                  -- we only do this after the object has been successfully
                  -- inserted in its new location.
                  removePrevious =
                     case previous of
                        Nothing -> -- this is presumably the first insertion
                           done
                        Just oldInsertion ->
                           do
                              let
                                 oldName = name oldInsertion
                              success <- applySimpleUpdate'
                                 (toContents oldInsertion)
                                 (\ map -> (delFromFM map oldName,
                                    elemFM oldName map))
                              when success (dirtyInsertion oldInsertion) 
               case insertion of
                  Nothing -> -- this must be a deletion
                     do
                        removePrevious
                        putMVar previousMVar insertion
                        return (hasValue ())
                  Just newInsertion ->
                     do
                        let
                           newName = name newInsertion

                        success <- applySimpleUpdate'
                           (toContents newInsertion)
                           (\ map -> 
                              if elemFM newName map
                                 then
                                    (map,False)
                                 else
                                    (addToFM map newName thisPtr,True)
                              )
                        if success 
                           then
                              do
                                 dirtyInsertion (newInsertion)
                                 dirtyLinkedObject linkedObject

                                 removePrevious
                                 putMVar previousMVar insertion
                                 broadcast insertionBroadcaster insertion
                                 return (hasValue ())
                           else
                              do
                                 putMVar previousMVar previous
                                 return (hasError (
                                    "There is already an object "++
                                    show (name newInsertion)++" in the folder"
                                    ))
            )

         dirtyInsertion :: Insertion -> IO ()
         dirtyInsertion insertion = dirtyLinkedObjectPtr (parent insertion)
               
         dirtyLinkedObjectPtr :: LinkedObjectPtr -> IO ()
         dirtyLinkedObjectPtr linkedObjectPtr = dirtyWrappedLink
            (wrappedLinkInPtr linkedObjectPtr)

         dirtyLinkedObject :: LinkedObject -> IO ()
         dirtyLinkedObject linkedObject 
            = dirtyWrappedLink (toWrappedLink linkedObject)

         dirtyWrappedLink :: WrappedLink -> IO ()
         dirtyWrappedLink (WrappedLink link) = dirtyLink view link

         linkedObject = LinkedObject {
            thisPtr = thisPtr,
            insertion = insertion,
            contents = contents1,
            moveObject = moveObject,
            importCommands = importCommands,
            contentsSource = contentsSource,
            importCommandsSource = importCommandsSource
            }

         thisPtr = LinkedObjectPtr {
            linkedObjectInPtr = linkedObject,
            wrappedLinkInPtr = wrappedLink
            }

      if isNew
         then
            do
               we <- moveObject insertion0
               return (mapWithError (\ () -> linkedObject) we)
         else
            return (hasValue linkedObject)


-- ----------------------------------------------------------------------
-- FrozenLinkedObject's
-- FrozenLinkedObject's are things that can be saved and restored
-- in the repository, to which LinkedObject's can be saved, and from
-- which LinkedObject's can be created.
-- ----------------------------------------------------------------------

data FrozenLinkedObject = FrozenLinkedObject {
   wrappedLink' :: WrappedLink,
   insertion' :: Maybe Insertion,
   contents' :: [(EntityName,LinkedObjectPtr)],
   hasImportCommands :: Bool
   } deriving (Eq,Typeable)

instance HasBinary FrozenLinkedObject CodingMonad where
   writeBin = mapWrite (\ (FrozenLinkedObject {wrappedLink' = wrappedLink,
      insertion' = insertion,contents' = contents,
      hasImportCommands = hasImportCommands})
      -> (wrappedLink,insertion,contents,hasImportCommands))
   readBin = mapRead (\ (wrappedLink,insertion,contents,hasImportCommands) ->
      (FrozenLinkedObject {wrappedLink' = wrappedLink,insertion' = insertion,
         contents' = contents,hasImportCommands = hasImportCommands}))

-- ----------------------------------------------------------------------
-- Instances of HasCodedValue via a FrozenLinkedObject
-- ----------------------------------------------------------------------

instance HasBinary LinkedObject CodingMonad where
   writeBin = mapWriteIO (\ linkedObject ->
      do
         frozenLinkedObject <- freezeLinkedObject linkedObject
         return frozenLinkedObject
      )
   readBin = mapReadViewIO (\ view frozenLinkedObject ->
      do
         linkedObjectWE <- createLinkedObject view False frozenLinkedObject
         return (coerceWithError linkedObjectWE)
      )
     
-- ----------------------------------------------------------------------
-- CodedValue for Insertion
-- ----------------------------------------------------------------------

instance HasBinary Insertion CodingMonad where
   writeBin = mapWrite (\ insertion -> (parent insertion,name insertion))
   readBin = mapRead (\ (parent,name) -> 
      Insertion {parent = parent,name = name})

-- ----------------------------------------------------------------------
-- LinkedObjectPtr
-- A LinkedObjectPtr is a LinkedObject together with its WrappedLink.
-- It is equivalent to a LinkedObject except that it has a different
-- definition of HasCodedValue, which uses a particularly foul trick
-- meaning that accessing the LinkedObject causes the object concerned to
-- be retrieved and the LinkedObject taken from it.  Thus LinkedObjectPtr's
-- are not suitable by the CodedValue instance for an object to encode
-- the object's LinkedObject (or you'll get circularity) but are suitable
-- for encoding references to LinkedObjects in other objects.
--
-- We include the WrappedLink in the LinkedObjectPtr so that a LinkedObjectPtr
-- can be decoded and encoded without having to reference the object
-- itself.
--
-- Each LinkedObject also stores its own LinkedObjectPtr, as thisPtr.
-- ----------------------------------------------------------------------

data LinkedObjectPtr = LinkedObjectPtr {
   linkedObjectInPtr :: LinkedObject,
   wrappedLinkInPtr :: WrappedLink
   }

fromLinkedObjectPtr :: LinkedObjectPtr -> LinkedObject
fromLinkedObjectPtr = linkedObjectInPtr

mkLinkedObjectPtr :: View -> WrappedLink -> LinkedObjectPtr
mkLinkedObjectPtr view wrappedLink =
   let
      action = extractLinkedObject wrappedLink view

      linkedObjectPtr = LinkedObjectPtr {
         linkedObjectInPtr = unsafePerformIO action,
         wrappedLinkInPtr = wrappedLink
         }
   in
      linkedObjectPtr


instance HasBinary LinkedObjectPtr CodingMonad where
   writeBin = mapWrite 
      (\ (LinkedObjectPtr {wrappedLinkInPtr = wrappedLink}) 
         -> wrappedLink
         )
   readBin = mapReadViewIO 
      (\ view wrappedLink ->
         return (mkLinkedObjectPtr view wrappedLink)
         )

instance Eq LinkedObjectPtr where
   (==) ptr1 ptr2 = wrappedLinkInPtr ptr1 == wrappedLinkInPtr ptr2

instance Ord LinkedObjectPtr where
   compare ptr1 ptr2 = compare (wrappedLinkInPtr ptr1) (wrappedLinkInPtr ptr2) 

extractLinkedObject :: WrappedLink -> View -> IO LinkedObject
extractLinkedObject (WrappedLink link) view =
   do
      object <- readLink view link
      return (fromMaybe (error "LinkManager.99") (toLinkedObjectOpt object))


lookupEntityName 
   :: LinkedObject -> EntityName -> SimpleSource (Maybe LinkedObjectPtr)
lookupEntityName linkedObject entityName =
   fmap
      (\ fm -> lookupFM fm entityName)
      (toSimpleSource (contents linkedObject))


-- ----------------------------------------------------------------------
-- FrozenLinkSource's and creating LinkSource's.
-- These do not (to avoid repetition) avoid the LinkedObject.
-- ----------------------------------------------------------------------

newtype FrozenLinkSource value = FrozenLinkSource {
   linksSource' :: [(EntitySearchName,value)]
   } deriving (Typeable)

instance HasBinary value CodingMonad 
   => HasBinary (FrozenLinkSource value) CodingMonad where

   writeBin = mapWrite 
      (\ (FrozenLinkSource {linksSource' = linksSource'})
         -> linksSource'
         )
   readBin = mapRead
      (\ linksSource' -> FrozenLinkSource {linksSource' = linksSource'})

freezeLinkSource :: LinkSource value -> IO (FrozenLinkSource value)
freezeLinkSource linkSource =
   do
      linksSource' <- readContents (linksSource linkSource)
      return (FrozenLinkSource {linksSource' = linksSource'})

createLinkSource :: View -> LinkedObject -> FrozenLinkSource value 
   -> IO (LinkSource value)
createLinkSource view linkedObject (FrozenLinkSource {
      linksSource' = linksSource'}) =
   createLinkSourceGeneral view linkedObject linksSource'
    
 
createLinkSourceGeneral :: View -> LinkedObject -> [(EntitySearchName,value)] 
   -> IO (LinkSource value)
createLinkSourceGeneral view linkedObject 
      (initialPairs :: [(EntitySearchName,value)]) =
   do
      linksBroadcaster <- newSimpleBroadcaster initialPairs

      (importsState :: ImportsState LinkedObject) <- getImportsState view

      let
         linksSource :: SimpleSource [(EntitySearchName,value)]
         linksSource = toSimpleSource linksBroadcaster

         setLinks newPairs = broadcast linksBroadcaster newPairs

         linksSource2 :: SimpleSource  [(LookupResult LinkedObject,value)]
         linksSource2 = mapIOSeq
            linksSource
            (\ searchData -> lookupNodes importsState linkedObject searchData)

         targetsSource :: SimpleSource [Maybe (LinkedObject,value)]
         targetsSource =
            fmap
               (\ (list :: [(LookupResult linkedObject,value)]) ->
                  map
                     (\ (lr,value) -> case lr of
                        Found linkedObject -> Just (linkedObject,value)
                        _ -> Nothing
                        )
                     list
                  )
               linksSource2

      return (
         LinkSource {
            object = linkedObject,
            linksSource = linksSource,
            targetsSource = targetsSource,
            setLinks = setLinks
            })

-- ----------------------------------------------------------------------
-- Functions for Merging
-- ----------------------------------------------------------------------

getLinkedObjectMergeLinks :: 
   (HasCodedValue object,HasLinkedObject object) 
   => MergeLinks object
getLinkedObjectMergeLinks = 
   let
      fn :: 
        (HasCodedValue object,HasLinkedObject object) 
        => View -> Link object -> IO (ObjectLinks EntityName)
      -- We use the Nothing key to indicate the object in which this object
      -- is inserted (if any).

      fn view objectLink =
         do
            object <- readLink view objectLink
            let
               linkedObject = toLinkedObject object

            (contents1 :: FiniteMap EntityName LinkedObjectPtr)
               <- readContents (contents linkedObject)
            let
               contents2 :: [(EntityName,LinkedObjectPtr)]
               contents2 = fmToList contents1

               contents3 :: [(WrappedMergeLink,EntityName)]
               contents3 =
                  map
                     (\ (entityName,linkedObjectPtr) ->
                        (toWrappedMergeLink (wrappedLinkInPtr linkedObjectPtr),
                           entityName)
                        )
                     contents2

            return (ObjectLinks contents3)
   in
      MergeLinks fn

   

---
-- The Link object gives the link where the LinkedObject is to be.
attemptLinkedObjectMerge :: ObjectType objectType object
   => MergeTypes.LinkReAssigner -> View -> 
   Link object -> [(View,LinkedObject)] -> IO (WithError LinkedObject)
attemptLinkedObjectMerge linkReAssigner newView targetLink sourceLinkedObjects
   = addFallOutWE (\ break ->
      do
         -- (1) freeze each of the source linked objects.
         (frozenLinkedObjects :: [(View,FrozenLinkedObject)]) 
            <- mapM
               (\ (view,linkedObject) ->
                  do
                     frozenLinkedObject <- freezeLinkedObject linkedObject
                     return (view,frozenLinkedObject)
                  )
               sourceLinkedObjects
 
         let
            -- (2) wrappedLink' field of new FrozenLinkedObject
            newWrappedLink' = WrappedLink targetLink

            -- (3) function for mapping a LinkedObjectPtr in a view to
            --     one in the target.
            mapLinkedObjectPtr :: View -> LinkedObjectPtr -> LinkedObjectPtr
            mapLinkedObjectPtr view linkedObjectPtr =
               let
                  wrappedLink0 = wrappedLinkInPtr linkedObjectPtr

                  wrappedMergeLink1 = lookupWithDefaultFM
                     (linkMap linkReAssigner)
                     (error ("LinkManager.mapLinkedObjectPtr "
                        ++ "- unassigned pointer??"))
                     (viewId view,toWrappedMergeLink wrappedLink0)

                  wrappedLink1 
                     = fromWrappedMergeLink wrappedLink0 wrappedMergeLink1
               in
                  mkLinkedObjectPtr newView wrappedLink1
                 
            -- (4) construct Insertions
            insertions :: [Maybe Insertion]
            (insertions @ (newInsertion' : restInsertions)) =
               map
                  (\ (view,frozenLinkedObject) ->
                     let
                        thisInsertionOpt = insertion' frozenLinkedObject
                     in
                        fmap
                           (\ insertion ->
                              let
                                 newParent = mapLinkedObjectPtr view 
                                   (parent insertion)
                              in
                                 Insertion {
                                    parent = newParent,
                                    name = name insertion
                                    }
                              )

                        thisInsertionOpt
                     )
                  frozenLinkedObjects

         if all (== newInsertion') restInsertions 
            then
               done
            else
               break "Merge failure; inconsistent insertions??"

         let
            -- (5) construct ImportCommands.
            hasImportCommandsList :: [Bool]
            hasImportCommandsList =
               map
                  (\ (_,frozen) -> hasImportCommands frozen)
                  frozenLinkedObjects

         hasImportCommands <- case allSame id hasImportCommandsList of
            Nothing -> break
               "LinkManager: importCommand status could not be determined"
            Just b -> return b

         let         
            -- (5) Construct contents.  Since we specified that these links
            -- had to be consistent during getLinkedObjectMergeLinks, we don't
            -- need to do that now.
            addContents :: View -> [(EntityName,LinkedObjectPtr)]
               -> FiniteMap EntityName LinkedObjectPtr
               -> FiniteMap EntityName LinkedObjectPtr
            -- add the contents for one FrozenLinkedObject to an existing
            -- map.
            addContents view theseContents map0 =
               foldl 
                  (\ map0 (entityName,linkedObjectPtr) ->
                     addToFM map0 entityName 
                        (mapLinkedObjectPtr view linkedObjectPtr)
                     )
                  map0
                  theseContents
            finalMap =
               foldl
                  (\ map0 (view,frozenLinkedObject) 
                     -> addContents view (contents' frozenLinkedObject) map0)
                  emptyFM
                  frozenLinkedObjects

            newContents' = fmToList finalMap

            newFrozenLinkedObject = FrozenLinkedObject {
               wrappedLink' = newWrappedLink',
               insertion' = newInsertion',
               contents' = newContents',
               hasImportCommands = hasImportCommands
               }

         linkedObjectWE 
            <- createLinkedObject newView False newFrozenLinkedObject

         coerceWithErrorOrBreakIO break linkedObjectWE
      )

---
-- Returns true if the CodedValue representations of the
-- two LinkedObject's are identical.  (The LinkedObject's are 
-- likely enough in different views.)
linkedObjectsSame :: LinkedObject -> LinkedObject -> IO Bool
linkedObjectsSame linkedObject1 linkedObject2 =
   do
      frozen1 <- freezeLinkedObject linkedObject1
      frozen2 <- freezeLinkedObject linkedObject2
      return (frozen1 == frozen2)
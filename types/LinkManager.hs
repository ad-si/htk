module LinkManager(
   -- Classes
   HasLinkedObject(..),
      -- those things which have a LinkedObject.  (No instances are
      -- declared in this file.)
   -- Types
   LinkedObject, 
      -- This corresponds to some object in the repository.
      -- Instance of HasCodedValue, Eq, Ord.
   LinkEnvironment,
      -- This represents some context (a path and an object) in which
      -- links are to be looked up.
   LinkSource,
      -- This represents a set of links (EntityFullName's) to search for.
      -- We also carry (with the LinkSource) values of some type specified
      -- as a parameter to LinkSource.

   Insertion(..),
      -- This represents somewhere to put a LinkedObject.
   LinkSourceSet(..),
      -- A LinkEnvironment and list of LinkSource's of the same type.
      -- Instance of HasCodedValue, provided the LinkSource type is.

   -- LinkedObject Functions
   newLinkedObject, 
      -- :: View -> WrappedLink -> Maybe Insertion 
      -- -> IO (WithError LinkedObject)
      -- Create a new LinkedObject.
   moveObject,
      -- :: LinkedObject -> Maybe Insertion -> IO (WithError ())
      -- Change the location of a LinkedObject
      -- Nothing means take the LinkedObject out of its parent and not
      -- put it anywhere else.
   mkInsertion,
      -- :: LinkedObject -> EntityName -> Insertion
      -- Make an insertion
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

   lookupObjectContents,
      -- :: LinkedObject -> EntityName -> SimpleSource (Maybe WrappedLink)
      -- Get a SimpleSource item corresponding to the EntityName element of
      -- the object contents.

   lookupNameSimple,
      -- :: LinkedObject -> String -> IO (Maybe LinkedObject)
      -- Extract a single element in a linkedObject's contents by name.
   lookupFullNameInFolder,
      -- :: LinkedObject -> EntityFullName -> IO (Maybe LinkedObject)
      -- Extract a full name as a sub object of a given object.
 
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

   -- LinkEnvironment functions
   newLinkEnvironment,
      -- :: LinkedObject -> EntityPath -> IO LinkEnvironment
      -- Create a new LinkEnvironment
   newParentLinkEnvironment,
      -- :: LinkedObject -> EntityPath -> IO LinkEnvironment
      -- Create a LinkEnvironment, where paths are relative to the objects
      -- parent.
   setPath,
      -- :: LinkEnvironment -> EntityPath -> IO ()
      -- Set a LinkEnvironment's path.
   lookupName,
      -- :: LinkEnvironment -> EntityFullName -> IO (Maybe LinkedObject)
      -- Return the LinkedObject corresponding to a LinkEnvironment, if it
      -- exists.

   lookupObject,
      -- :: ObjectType objectType object => LinkEnvironment 
      -- -> EntityFullName -> IO (WithError (Maybe (Link object)))
      -- Looks up an object, returning the actual link if possible.
      -- We return Nothing if the object does not exist but cause an error if 
      -- the object has the wrong type.

   lookupObjectByPath,
      -- :: ObjectType objectType object => LinkedObject -> EntityPath 
      -- -> EntityFullName -> IO (WithError (Maybe (Link object)))
      -- Looks up an object, returning the actual link if possible,
      -- starting with an EntityPath rather than a LinkEnvironment.
      -- We return Nothing if the object does not exist but cause an error if 
      -- the object has the wrong type.

   -- LinkSource functions
   newLinkSource, 
      -- :: LinkEnvironment -> [(EntityFullName,value)] 
      -- -> IO (LinkSource value)
      -- Create a new LinkSource
   setLinks,
      -- :: LinkSource value -> [(EntityFullName,value)] -> IO ()
      -- Set the links for the LinkSource.
   listFromLinkSource,
      -- :: LinkSource value -> SimpleSource [(LinkedObject,value)]
      -- Obtain the pointed-to elements for a LinkSource.
   verifyLinkSource,
      -- :: LinkSource value -> IO [(EntityFullName,value)]
      -- Return the EntityFullName's which cannot be matched for the 
      -- LinkSource.

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
import VariableMap
import VariableSet
import VariableList
import AtomString(fromStringWE,toString)

import DialogWin

import LinkDrawer
import CodedValue
import ObjectTypes
import EntityNames
import Link
import ViewType
import View
import MergeTypes

-- ----------------------------------------------------------------------
-- User interface
-- ----------------------------------------------------------------------

-- those things which have a LinkedObject.  (No instances are
-- declared in this file.)
class HasLinkedObject object where
   toLinkedObject :: object -> LinkedObject

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
            contents' = []
            }
      createLinkedObject view True frozenLinkedObject

---
-- Get the contents of a LinkedObject (those objects contained in it,
-- like elements in a folder).
objectContents :: LinkedObject -> VariableSetSource WrappedLink
objectContents linkedObject =
   mapToVariableSetSource 
      (\ _ linkedObjectPtr -> wrappedLinkInPtr linkedObjectPtr)
      (contents linkedObject) 

--
-- Get a SimpleSource item corresponding to the EntityName element of
-- the object contents.
lookupObjectContents :: LinkedObject -> EntityName 
   -> SimpleSource (Maybe WrappedLink)
lookupObjectContents linkedObject entityName =
   fmap
      (fmap wrappedLinkInPtr)
      (getVariableMapByKey (contents linkedObject) entityName)



---
-- Delete an object including its record in the view and the parent
-- folder and anywhere else (currently nowhere) where the Link is
-- stored by the LinkManager, EXCEPT in LinkEnvironments, which should
-- be disposed of by the caller. 
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
                        createErrorWin mess []
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
                        createErrorWin mess []
                        return Nothing
                     )
         )
      act



---
-- Extract a single element in a linkedObject's contents by name.
lookupNameSimple :: LinkedObject -> String -> IO (Maybe LinkedObject)
lookupNameSimple linkedObject str =
   do
      let
         entityNameWE = fromStringWE str
      case fromWithError entityNameWE of
         Left _ -> return Nothing
         Right entityName ->
            do
               conts <- readContents (contents linkedObject)
               let
                  linkedObjectPtrOpt = lookupMap conts entityName
               return (fmap fromLinkedObjectPtr linkedObjectPtrOpt)

---
-- Extract a full name as a sub object of a given object.
lookupFullNameInFolder :: LinkedObject -> EntityFullName 
   -> IO (Maybe LinkedObject)
lookupFullNameInFolder linkedObject entityFullName =
   do
      let
         source = mapOneName linkedObject trivialPath entityFullName
      readContents source

---
-- Make an insertion
mkInsertion :: LinkedObject -> EntityName -> Insertion
mkInsertion linkedObject entityName =
   Insertion {
      parent = thisPtr linkedObject,
      name = entityName
      }

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

---
-- Create a new LinkEnvironment
newLinkEnvironment :: LinkedObject -> EntityPath -> IO LinkEnvironment
newLinkEnvironment linkedObject path = 
   createLinkEnvironment (FrozenLinkEnvironment {
      linkedObject' = thisPtr linkedObject,path' = path})

---
-- Create a LinkEnvironment, where paths are relative to the objects
-- parent.
newParentLinkEnvironment :: LinkedObject -> EntityPath -> IO LinkEnvironment
newParentLinkEnvironment linkedObject entityPath =
   do
      linkEnvironment <- newLinkEnvironment linkedObject entityPath
      return (linkEnvironment {
         path = fmap raiseEntityPath (path linkEnvironment)
         })

---
-- Return the LinkedObject corresponding to a LinkEnvironment, if it
-- exists.
lookupName :: LinkEnvironment -> EntityFullName -> IO (Maybe LinkedObject)
lookupName linkEnvironment entityFullName =
   do
      entityPath <- readContents (path linkEnvironment)
      let
         source = mapOneName (linkedObject linkEnvironment) entityPath 
            entityFullName
      readContents source

---
-- Return the LinkedObjectPtr corresponding to a LinkEnvironment, if it
-- exists.
lookupNamePtr :: LinkEnvironment -> EntityFullName 
   -> IO (Maybe LinkedObjectPtr)
lookupNamePtr linkEnvironment entityFullName =
   do
      entityPath <- readContents (path linkEnvironment)
      let
         source = mapOneNamePtr 
            (thisPtr (linkedObject linkEnvironment)) entityPath entityFullName
      readContents source

---
-- Looks up an object, returning the actual link if possible.
-- We return Nothing if the object does not exist but cause an error if the
-- object has the wrong type.
lookupObject :: ObjectType objectType object 
   => LinkEnvironment -> EntityFullName -> IO (WithError (Maybe (Link object)))
lookupObject linkEnvironment entityFullName =
   addFallOutWE (\ break ->
      do
         linkedObjectPtrOpt <- lookupNamePtr linkEnvironment entityFullName
         unpackLinkedObjectPtr break linkedObjectPtrOpt
      )

---
-- Looks up an object, returning the actual link if possible,
-- starting with an EntityPath rather than a LinkEnvironment.
-- We return Nothing if the object does not exist but cause an error if 
-- the object has the wrong type.
lookupObjectByPath :: ObjectType objectType object 
   => LinkedObject -> EntityPath 
   -> EntityFullName -> IO (WithError (Maybe (Link object)))
lookupObjectByPath linkedObject entityPath entityFullName =
   addFallOutWE (\ break ->
      do
         linkedObjectPtrOpt 
            <- lookupNamePtrByPath linkedObject entityPath entityFullName
         unpackLinkedObjectPtr break linkedObjectPtrOpt
      )
        
---
-- Return the LinkedObjectPtr.
lookupNamePtrByPath :: LinkedObject -> EntityPath -> EntityFullName 
   -> IO (Maybe LinkedObjectPtr)
lookupNamePtrByPath linkedObject entityPath entityFullName =
   do
      let
         source = mapOneNamePtr 
            (thisPtr linkedObject) entityPath entityFullName
      readContents source

---
-- Used by lookupObject and lookupObjectByPath
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
newLinkSource :: LinkEnvironment -> [(EntityFullName,value)] 
  -> IO (LinkSource value)
newLinkSource = createLinkSourceGeneral

---
-- Obtain the pointed-to elements for a LinkSource.
listFromLinkSource :: LinkSource value -> SimpleSource [(LinkedObject,value)]
listFromLinkSource linkSource = 
   fmap catMaybes (targetsSource linkSource)

---
-- Return the EntityFullName's which cannot be matched for the LinkSource.
verifyLinkSource :: LinkSource value -> IO [EntityFullName]
verifyLinkSource (linkSource :: LinkSource value) =
   do
      (targets :: [Maybe (LinkedObject,value)])
         <- readContents (targetsSource linkSource)
      if any isNothing targets
         then
            do
               -- since targets doesn't tell us what the names are, we go
               -- through them one by one.   
               namePairs <- readContents (linksSource linkSource)
               let
                  names = map fst namePairs
               path <- readContents (path (environment linkSource))
               namesOpts <- mapM
                  (\ name ->
                     do
                        linkedOpt <- readContents 
                           (mapOneName 
                              (linkedObject (environment linkSource)) 
                              path name
                              )
                        return (case linkedOpt of
                           Nothing -> Just name
                           _ -> Nothing
                           )
                     ) 
                  names
               return (catMaybes namesOpts)
         else
            return []

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
   contents :: VariableMap EntityName LinkedObjectPtr,
   moveObject :: Maybe Insertion -> IO (WithError ())
   }

data LinkEnvironment = LinkEnvironment {
   linkedObject :: LinkedObject,
      -- ^ We do NOT delete the WrappedLink inside a LinkEnvironment when
      -- we do deleteLinkedObject for the corresponding object.  Thus it
      -- is necessary not to access the corresponding link with readLink 
      -- or fetchLink.
   path :: SimpleSource EntityPath,
   setPath :: EntityPath -> IO ()
   }

data Insertion = Insertion {
   parent :: LinkedObjectPtr, -- folder to contain this object
   name :: EntityName -- name it shall have
   } deriving (Eq)

data LinkSourceSet value = LinkSourceSet LinkEnvironment [LinkSource value]

data LinkSource value = LinkSource {
   environment :: LinkEnvironment,
   linksSource :: SimpleSource [(EntityFullName,value)],
   targetsSource :: SimpleSource [Maybe (LinkedObject,value)],
   setLinks :: [(EntityFullName,value)] -> IO ()
   }

-- ----------------------------------------------------------------------
-- Looking up names
-- ----------------------------------------------------------------------

mapOneName :: LinkedObject -> EntityPath -> EntityFullName 
   -> SimpleSource (Maybe LinkedObject)
mapOneName linkedObject path fullName =
   fmap
      (fmap linkedObjectInPtr)
      (mapOneNamePtr (thisPtr linkedObject) path fullName)

mapOneNamePtr :: LinkedObjectPtr -> EntityPath -> EntityFullName
   -> SimpleSource (Maybe LinkedObjectPtr)
mapOneNamePtr linkedObjectPtr (EntityPath []) fullName = return Nothing
mapOneNamePtr linkedObjectPtr (EntityPath (firstSearch:rest)) fullName =
   do
      linkedObjectPtrOpt 
         <- mapSearchNamePtr linkedObjectPtr firstSearch fullName
      case linkedObjectPtrOpt of
         Nothing -> mapOneNamePtr linkedObjectPtr (EntityPath rest) fullName
         _ -> return linkedObjectPtrOpt

mapSearchNamePtr :: LinkedObjectPtr -> EntitySearchName -> EntityFullName 
   -> SimpleSource (Maybe LinkedObjectPtr)
mapSearchNamePtr linkedObjectPtr (FromHere entityFullName) toSearch =
   mapFullNameFullNamePtr linkedObjectPtr entityFullName toSearch
mapSearchNamePtr linkedObjectPtr (FromParent entitySearchName) toSearch =
   do
      insertionOpt 
         <- toSimpleSource (insertion (linkedObjectInPtr linkedObjectPtr))
      case insertionOpt of
         Nothing -> return Nothing
         Just insertion ->
            mapSearchNamePtr (parent insertion) entitySearchName toSearch      

---
-- Search for the second EntityFullName down the path given by the first
-- EntityFullName
mapFullNameFullNamePtr :: LinkedObjectPtr -> EntityFullName -> EntityFullName 
   -> SimpleSource (Maybe LinkedObjectPtr)
mapFullNameFullNamePtr linkedObjectPtr (EntityFullName n1) (EntityFullName n2)
   = mapFullNamePtr linkedObjectPtr (EntityFullName (n1 ++ n2))

---
-- Search for the EntityFullName searching from the given object
mapFullNamePtr :: LinkedObjectPtr -> EntityFullName 
   -> SimpleSource (Maybe LinkedObjectPtr)
mapFullNamePtr linkedObjectPtr (EntityFullName []) 
   = return (Just linkedObjectPtr)
mapFullNamePtr linkedObjectPtr (EntityFullName (first:rest)) =
   do
      linkedObjectPtrOpt <- getVariableMapByKey 
         (contents (linkedObjectInPtr linkedObjectPtr)) 
         first
      case linkedObjectPtrOpt of
         Nothing -> return Nothing
         Just linkedObjectPtr -> 
            mapFullNamePtr linkedObjectPtr (EntityFullName rest)

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
         (contents' :: [(EntityName,LinkedObjectPtr)]) = mapToList contentsData
      return (FrozenLinkedObject {wrappedLink' = wrappedLink',
         insertion' = insertion',contents' = contents'})

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
      let
         insertion = toSimpleSource insertionBroadcaster

      contents1 <- newVariableMap (contents' frozenLinkedObject)

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
                              success <- delFromVariableMap 
                                 (toContents oldInsertion)
                                 (name oldInsertion)
                              debugTest success "LinkManager.1"
               case insertion of
                  Nothing -> -- this must be a deletion
                     do
                        removePrevious
                        putMVar previousMVar insertion
                        return (hasValue ())
                  Just newInsertion ->
                     do
                        success <- addToVariableMap
                           (toContents newInsertion)
                           (name newInsertion)
                           thisPtr
                        if success 
                           then
                              do
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

         linkedObject = LinkedObject {
            thisPtr = thisPtr,
            insertion = insertion,
            contents = contents1,
            moveObject = moveObject
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
   contents' :: [(EntityName,LinkedObjectPtr)]
   }

frozenLinkedObject_tyRep = mkTyRep "LinkManager" "FrozenLinkedObject"
instance HasTyRep FrozenLinkedObject where
   tyRep _ = frozenLinkedObject_tyRep

instance HasCodedValue FrozenLinkedObject where
   encodeIO = mapEncodeIO (\ (FrozenLinkedObject {wrappedLink' = wrappedLink,
      insertion' = insertion,contents' = contents})
      -> (wrappedLink,insertion,contents))
   decodeIO = mapDecodeIO (\ (wrappedLink,insertion,contents) ->
      (FrozenLinkedObject {wrappedLink' = wrappedLink,insertion' = insertion,
         contents' = contents}))

-- ----------------------------------------------------------------------
-- Instances of Typeable and HasCodedValue via a FrozenLinkedObject
-- ----------------------------------------------------------------------

linkedObject_tyRep = mkTyRep "LinkManager" "LinkedObject"
instance HasTyRep LinkedObject where
   tyRep _ = linkedObject_tyRep

linkSource_tyRep = mkTyRep "LinkManager" "LinkSource"
instance HasTyRep1 LinkSource where
   tyRep1 _ = linkSource_tyRep

instance HasCodedValue LinkedObject where
   encodeIO linkedObject codedValue view =
      do
         frozenLinkedObject <- freezeLinkedObject linkedObject
         encodeIO frozenLinkedObject codedValue view
   decodeIO codedValue0 view =
      do
         (frozenLinkedObject,codedValue1) <- decodeIO codedValue0 view
         linkedObjectWE <- createLinkedObject view False frozenLinkedObject
         return (coerceWithError linkedObjectWE,codedValue1)
     
-- ----------------------------------------------------------------------
-- CodedValue for Insertion
-- ----------------------------------------------------------------------

insertion_tyRep = mkTyRep "LinkManager" "Insertion"
instance HasTyRep Insertion where
   tyRep _ = insertion_tyRep

instance HasCodedValue Insertion where
   encodeIO = mapEncodeIO (\ insertion -> (parent insertion,name insertion))
   decodeIO = mapDecodeIO (\ (parent,name) -> 
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
-- can be decodeIO'd and encodeIO'd without having to reference the object
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


linkedObjectPtr_tyRep = mkTyRep "LinkManager" "LinkedObjectPtr"
instance HasTyRep LinkedObjectPtr where
   tyRep _ = linkedObjectPtr_tyRep

instance HasCodedValue LinkedObjectPtr where
   encodeIO (LinkedObjectPtr {wrappedLinkInPtr = wrappedLink})
         codedValue0 view = encodeIO wrappedLink codedValue0 view
   decodeIO codedValue0 view =
      do
         (wrappedLink,codedValue1) <- decodeIO codedValue0 view
         let
            linkedObjectPtr = mkLinkedObjectPtr view wrappedLink
         return (linkedObjectPtr,codedValue1)



instance Eq LinkedObjectPtr where
   (==) ptr1 ptr2 = wrappedLinkInPtr ptr1 == wrappedLinkInPtr ptr2

instance Ord LinkedObjectPtr where
   compare ptr1 ptr2 = compare (wrappedLinkInPtr ptr1) (wrappedLinkInPtr ptr2) 

extractLinkedObject :: WrappedLink -> View -> IO LinkedObject
extractLinkedObject (WrappedLink link) view =
   do
      object <- readLink view link
      return (fromMaybe (error "LinkManager.99") (toLinkedObjectOpt object))

-- ----------------------------------------------------------------------
-- FrozenLinkEnvironment's and LinkEnvironment's.
-- ----------------------------------------------------------------------

data FrozenLinkEnvironment = FrozenLinkEnvironment {
   linkedObject' ::  LinkedObjectPtr,
   path' :: EntityPath
   }

frozenLinkEnvironment_tyRep = mkTyRep "LinkManager" "FrozenLinkEnvironment"

instance HasTyRep FrozenLinkEnvironment where
   tyRep _ = frozenLinkEnvironment_tyRep

instance HasCodedValue FrozenLinkEnvironment where
   encodeIO = mapEncodeIO (\ 
      (FrozenLinkEnvironment {linkedObject' = linkedObject',path' = path'})
      -> (linkedObject',path')
      )
   decodeIO = mapDecodeIO (\ (linkedObject',path') ->
      (FrozenLinkEnvironment {linkedObject' = linkedObject',path' = path'})
      )

linkEnvironment_tyRep = mkTyRep "LinkManager" "LinkEnvironment"

instance HasTyRep LinkEnvironment where
   tyRep _ = linkEnvironment_tyRep

freezeLinkEnvironment :: LinkEnvironment -> IO FrozenLinkEnvironment 
freezeLinkEnvironment 
      (LinkEnvironment {linkedObject = linkedObject,path = path}) =
   do
      path' <- readContents path
      return (FrozenLinkEnvironment {
         linkedObject' = thisPtr linkedObject,path' = path'})

createLinkEnvironment :: FrozenLinkEnvironment -> IO LinkEnvironment
createLinkEnvironment (FrozenLinkEnvironment {linkedObject' = linkedObject',
      path' = path'}) =
   do
      let
         linkedObject = fromLinkedObjectPtr linkedObject' 
      path0 <- newSimpleBroadcaster path'
      let
         path = uniqSimpleSource (toSimpleSource path0)
         setPath newPath = broadcast path0 newPath
      return (LinkEnvironment {linkedObject = linkedObject,path = path,
         setPath = setPath})

-- ----------------------------------------------------------------------
-- FrozenLinkSource's and creating LinkSource's.
-- These do not (to avoid repetition) contain the LinkEnvironment.
-- ----------------------------------------------------------------------

newtype FrozenLinkSource value = FrozenLinkSource {
   linksSource' :: [(EntityFullName,value)]
   }

frozenLinkSource_tyRep = mkTyRep "LinkManager" "FrozenLinkSource"

instance HasTyRep1 FrozenLinkSource where
   tyRep1 _ = frozenLinkSource_tyRep

instance HasCodedValue value => HasCodedValue (FrozenLinkSource value) where
   encodeIO = mapEncodeIO (\ (FrozenLinkSource {linksSource' = linksSource'})
      -> linksSource')
   decodeIO = mapDecodeIO (\ linksSource'
      -> FrozenLinkSource {linksSource' = linksSource'})

freezeLinkSource :: LinkSource value -> IO (FrozenLinkSource value)
freezeLinkSource linkSource =
   do
      linksSource' <- readContents (linksSource linkSource)
      return (FrozenLinkSource {linksSource' = linksSource'})

createLinkSource :: LinkEnvironment -> FrozenLinkSource value 
   -> IO (LinkSource value)
createLinkSource linkEnvironment (FrozenLinkSource {
      linksSource' = linksSource'}) =
   createLinkSourceGeneral linkEnvironment linksSource'
    
 
createLinkSourceGeneral :: LinkEnvironment -> [(EntityFullName,value)] 
   -> IO (LinkSource value)
createLinkSourceGeneral linkEnvironment initialPairs =
   do
      let
         pathSource = path linkEnvironment
      linksBroadcaster <- newSimpleBroadcaster initialPairs
      let
         linksSource = toSimpleSource linksBroadcaster

         setLinks newPairs = broadcast linksBroadcaster newPairs

         targetsSource =
            do
               (path,links) <- pairSimpleSources pathSource linksSource
               sequenceSimpleSource (map 
                  (\ (name,value) ->
                     fmap
                        (fmap (\ link -> (link,value)))
                        (mapOneName (linkedObject linkEnvironment) path name)
                     )
                  links
                  )
      return (
         LinkSource {
            environment = linkEnvironment,
            linksSource = linksSource,
            targetsSource = targetsSource,
            setLinks = setLinks
            })

-- ----------------------------------------------------------------------
-- A LinkSourceSet contains a LinkEnvironment plus a number of LinkSource's,
-- and is a group of things that can be encoded and decoded together.
-- ----------------------------------------------------------------------

linkSourceSet_tyRep = mkTyRep "LinkManager" "LinkSourceSet"
instance HasTyRep1 LinkSourceSet where
   tyRep1 _ = linkSourceSet_tyRep

instance HasCodedValue value => HasCodedValue (LinkSourceSet value) where
   encodeIO (LinkSourceSet linkEnvironment linkSources) codedValue view =
      do
         frozenLinkEnvironment <- freezeLinkEnvironment linkEnvironment
         frozenLinkSources <- mapM freezeLinkSource linkSources
         encodeIO (frozenLinkEnvironment,frozenLinkSources) codedValue view
   decodeIO codedValue0 view =
      do
         ((frozenLinkEnvironment,frozenLinkSources),codedValue1) <-
            decodeIO codedValue0 view
         linkEnvironment <- createLinkEnvironment frozenLinkEnvironment
         linkSources <- mapM (createLinkSource linkEnvironment)
            frozenLinkSources
         return (LinkSourceSet linkEnvironment linkSources,codedValue1)


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
               
               contents0 :: VariableMap EntityName LinkedObjectPtr
               contents0 = contents linkedObject

            contents1 <- readContents contents0

            let
               contents2 :: [(EntityName,LinkedObjectPtr)]
               contents2 = mapToList contents1

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
                     -> addContents view (contents' frozenLinkedObject) map0
                     )
                  emptyFM
                  frozenLinkedObjects

            newContents' = fmToList finalMap

            newFrozenLinkedObject = FrozenLinkedObject {
               wrappedLink' = newWrappedLink',
               insertion' = newInsertion',
               contents' = newContents'
               }

         linkedObjectWE 
            <- createLinkedObject newView False newFrozenLinkedObject

         coerceWithErrorOrBreakIO break linkedObjectWE
      )
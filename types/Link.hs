{- This module defines links, which are pointers to objects in the repository.
   Thus they can be part of other objects. -}
module Link(
   -- Links.
   -- A Link x is a pointer to an object of type x.  Links are made
   -- instances of HasCodedValue, which means they
   -- can themselves be stored in the repository, for example as attributes
   -- of other objects.
   Link, -- instance of Eq,Ord.

   topLink, -- :: Link x
      -- This link points to the "top object".  This needs to be
      -- created 

   setTopLink, -- :: HasCodedValue x => View -> x -> IO (Versioned x)
      -- This initialises the top link to a particular value.  It should
      -- only be used once, in setting up the repository.

   setOrGetTopLink, -- :: HasCodedValue x => View -> IO x -> IO (Versioned x)
      -- setOrGetTopLink is somewhat safer than setTopLink and initialises the
      -- top object, if that hasn't already been done, via the supplied action.
      -- Otherwise it (harmlessly) returns the existing object.

   -- Versioned values
   -- A Versioned x is a box containing an actual x which can be
   -- stored in the repository.  The x needs to be an instance of 
   -- HasCodedValue.
   Versioned,

   createObject, -- :: HasCodedValue x => View -> x -> IO (Versioned x) 
   -- This is used for creating a completely new object.

   deleteLink, -- :: HasCodedValue x => View -> Link x -> IO ()
   -- This deletes an object from the View.
   -- NB.  It is very important to make sure that the object is not later
   -- dereferenced by fetchLink or readLink, or you will simply get a crash.
   -- 
   -- The LinkManager.deleteLinkedObject takes care of this for any links
   -- it stores.

   cloneLink, -- :: HasCodedValue x => View -> Link x -> View -> IO ()
   -- Copy a link with its contents from the old to the new view.

   newEmptyObject, -- :: HasCodedValue x => View -> IO (Versioned x)
   -- This creates an object with no contents as a stop-gap so you can
   -- create a link to it, EG for constructing circular lists.
   -- WARNING - updateObject must be used to put in an actual value,
   -- before readObject is done on this Versioned value.  This must also be
   -- done before any commitView, unless the object is deleted via deleteLink.

   updateObject, -- :: HasCodedValue x => View -> x -> Versioned x -> IO ()
   -- This replaces the x value inside an object by a new value, and
   -- marks it to be stored anew.
   dirtyObject, -- :: HasCodedValue x => View -> Versioned x -> IO ()
   -- This marks the x value to be stored anew, even though it hasn't
   -- been changed.  This is needed, for example, when x points to a file,
   -- which has been updated on the side.
   readObject, -- :: HasCodedValue x => View -> Versioned x -> IO x
   -- get the current contents of the object.


   makeLink, -- :: HasCodedValue x => View -> Versioned x -> IO (Link x)
   -- makeLink is used to create a link to an object.
   fetchLink, -- :: HasCodedValue x => View -> Link x -> IO (Versioned x)
   -- look up a link to an object in the repository.
   readLink, -- :: HasCodedValue x => View -> Link x -> IO x
   -- Does fetchLink and readObject in one go.
   writeLink, -- :: HasCodedValue x => View -> Link x -> x -> IO ()
   -- Does fetchLink and updateObject in one go.
   createLink, -- :: HasCodedValue x => View -> x -> IO (Link x)
   -- Does createObject and makeLink in one go.
   newEmptyLink, -- :: HasCodedValue x => View -> IO (Link x)
   -- Like newEmptyObject; similar considerations apply.

   absolutelyNewLink, -- :: HasCodedValue x => Repository -> IO (Link x)
   -- Allocate a new link but don't put it in any view.

   dirtyLink, -- :: HasCodedValue x => View -> Link x -> IO ()
   -- Does fetchLink and dirtyObject in one go.

   fetchLinkWE, -- :: HasCodedValue x => View -> Link x 
      -- -> IO (WithError(Versioned x))
      -- Like fetchLink but should not crash for deleted links.
   readLinkWE, -- :: HasCodedValue x => View -> Link x -> IO (WithError x)
      -- Like readLink but should not crash for deleted links.


   eqLink, -- :: Link x -> Link x -> Ordering
   compareLink, -- :: Link x -> Link y -> Ordering
      -- Provide an efficient way of testing two links for equality, and
      -- ordering them.


   getLastChange, 
      -- :: View -> Link object -> IO (Maybe ObjectVersion)
      -- Return the last-change indicator for a link in the view.

   setLink,
      -- :: HasCodedValue x => View -> x -> Link x -> IO (Versioned x)
      -- Set the contents of a pre-allocated link to a particular value.
      -- NB.  This function is only intended for use for merging.  The
      -- link should not have anything in it before.

   ) where

import Control.Concurrent
import Data.IORef

import Registry
import Dynamics
import AtomString(fromString)
import VariableSet(HasKey(..))
import Computation

import VersionDB
import ViewType
import CodedValue
import CodedValueStore

-- ----------------------------------------------------------------------
-- Links
-- ----------------------------------------------------------------------

newtype Link x = Link Location deriving (Eq,Ord)

link_tyRep = mkTyRep "Link" "Link"
instance HasTyRep1 Link where
   tyRep1 _ = link_tyRep

instance Typeable x => HasCodedValue (Link x) where
   encodeIO = mapEncodeIO (\ (Link location) -> Str location)
   decodeIO = mapDecodeIO (\ (Str location) -> Link location)

instance HasKey (Link x) Location where
   toKey (Link location) = location

topLink :: Link x
topLink = Link secondLocation

makeLink :: HasCodedValue x => View -> Versioned x -> IO (Link x)
makeLink _ (Versioned {location = location}) = return (Link location)

fetchLink :: HasCodedValue x => View -> Link x -> IO (Versioned x)
fetchLink view link =
   do
      versionedWE <- fetchLinkWE view link
      return (coerceWithError versionedWE)

fetchLinkWE :: HasCodedValue x => View -> Link x 
   -> IO (WithError (Versioned x))
fetchLinkWE (view@View{repository = repository,objects = objects}) 
      (Link location) =
   do
      transformValue objects location 
         (\ objectDataOpt ->
          do
            let
               err mess = return (objectDataOpt,hasError mess) 
            case objectDataOpt of
               Nothing -> err "View.fetchLink: Link to object not in view!!"
               Just (PresentObject {thisVersioned = versionedDyn}) ->
                  case fromDyn versionedDyn of
                     Just versioned 
                        -> return (objectDataOpt,hasValue versioned)
                     Nothing -> err "View.fetchLink - type error in link"
               Just (AbsentObject {
                     thisObjectVersion = objectVersion,
                     lastChange = lastChange
                     }) ->
                  do
                     -- create a new Versioned object
                     (str :: String) <- 
                        retrieveString repository location objectVersion
                     x <- doDecodeIO (fromString str) view
                     statusMVar <- newMVar (UpToDate x objectVersion)
                     let
                        versioned = Versioned {
                           location = location,
                           statusMVar = statusMVar
                           }
                     return (Just(
                        PresentObject {
                           thisVersioned = toDyn versioned,
                           commitAct = commitVersioned view versioned,
                           lastChange = lastChange
                           }),
                        hasValue versioned)
            )                 

readLink :: HasCodedValue x => View -> Link x -> IO x
readLink view link =
   do
      versioned <- fetchLink view link
      readObject view versioned 

writeLink :: HasCodedValue x => View -> Link x -> x -> IO ()
writeLink view link x =
   do
      versioned <- fetchLink view link
      updateObject view x versioned

createLink :: HasCodedValue x => View -> x -> IO (Link x)
createLink view x =
   do
      versioned <- createObject view x
      makeLink view versioned

newEmptyLink :: HasCodedValue x => View -> IO (Link x)
newEmptyLink view =
   do
      versioned <- newEmptyObject view
      makeLink view versioned

absolutelyNewLink :: HasCodedValue x => Repository -> IO (Link x)
absolutelyNewLink repository =
   do
      location <- newLocation repository
      return (Link location)


dirtyLink :: HasCodedValue x => View -> Link x -> IO ()
dirtyLink view link =
   do
      versioned <- fetchLink view link
      dirtyObject view versioned

readLinkWE :: HasCodedValue x => View -> Link x -> IO (WithError x)
readLinkWE view link =
   do
      versionedWE <- fetchLinkWE view link
      mapWithErrorIO'
         (\ versioned ->
            do
               object <- readObject view versioned
               return (hasValue object)
            )
         versionedWE


eqLink :: Link x -> Link y -> Bool
eqLink (Link loc1) (Link loc2) = loc1 == loc2

compareLink :: Link x -> Link y -> Ordering
compareLink (Link loc1) (Link loc2) = compare loc1 loc2

cloneLink :: HasCodedValue x => View -> Link x -> View -> IO ()
cloneLink oldView link newView =
   do
      oldVersioned <- fetchLink oldView link
      cloneObject oldView oldVersioned newView
      done

-- ----------------------------------------------------------------------
-- Versioned
-- ----------------------------------------------------------------------

data Versioned x = Versioned {
   location :: Location, -- Location in the view.
   statusMVar :: MVar (Status x)
   }

-- Make Versioned objects typeable (if x is).
versioned_tyRep = mkTyRep "View" "Versioned"

instance HasTyRep1 Versioned where
   tyRep1 _ = versioned_tyRep

commitVersioned :: HasCodedValue x => View -> Versioned x -> ObjectVersion
   -> IO ObjectVersion
-- This is the action that gets done when we commit an object to
-- the repository.
--
-- The supplied objectVersion is that of the containing view.
commitVersioned (view@View{repository = repository})
      (Versioned{location = location,statusMVar = statusMVar}) 
      viewVersion =
   do
      status <- takeMVar statusMVar
      let 
         commitX x parentVersionOpt =
            do
               xCodedValue <- doEncodeIO x view
               xObjectSource <- toObjectSource xCodedValue
               xObjectVersion <- commit repository xObjectSource location 
                  parentVersionOpt
               updateLastChange view location viewVersion
               return (x,xObjectVersion)
      (x,objectVersion) <- case status of
         Empty -> error "Attempt to commit Empty object!!!"
         UpToDate x objectVersion -> return (x,objectVersion)
         Dirty x objectVersion -> commitX x (Just objectVersion)
         Virgin x -> commitX x Nothing
      putMVar statusMVar (UpToDate x objectVersion)
      return objectVersion

data Status x = 
      Empty -- created by newEmptyObject
   |  UpToDate x ObjectVersion -- This object committed and up-to-date
   |  Dirty x ObjectVersion -- This object committed, but since modified
   |  Virgin x -- Object never committed.

setTopLink :: HasCodedValue x => View -> x -> IO (Versioned x)
setTopLink view x = setLink view x (Link secondLocation)

setLink :: HasCodedValue x => View -> x -> Link x -> IO (Versioned x)
setLink view x (Link location) = createObjectGeneral view (Virgin x) location

---
-- setOrGetTopLink is somewhat safer than setTopLink and initialises the
-- top object, if that hasn't already been done, via the supplied action.
-- Otherwise it (harmlessly) returns the existing object.
setOrGetTopLink :: HasCodedValue x => View -> IO x -> IO (Versioned x)
setOrGetTopLink (view@View{repository = repository,objects = objects}) action =
   do
      -- We delay using versionedAct doing things that don't need to be
      -- done inside transformValue, the reason being that then we
      -- can use fetchLink (which needs transformValue to have finished).
      versionedAct <- transformValue objects secondLocation
         (\ objectDataOpt ->
            case objectDataOpt of
               Nothing ->
                  do
                     -- Not in repository, create.
                     x <- action
                     (versioned,objectData) <-
                         makeObjectData view (Virgin x) secondLocation
                     return (Just objectData,return versioned)
               Just objectData ->
                do
                  -- Is in repository.  So we just return something
                  -- to get the topLink
                  return (Just objectData,fetchLink view topLink)
            )
      versionedAct   

createObject :: HasCodedValue x => View -> x -> IO (Versioned x)
createObject view x =
   do
      location <- newLocation (repository view)
      createObjectGeneral view (Virgin x) location

newEmptyObject :: HasCodedValue x => View -> IO (Versioned x)
newEmptyObject view = 
   do
      location <- newLocation (repository view)
      createObjectGeneral view Empty location

createObjectGeneral :: HasCodedValue x => View -> Status x -> Location 
   -> IO (Versioned x)
-- createObjectGeneral creates a completely new object for an already-
-- allocated location, given a Status (Virgin or Empty) to put in it.
createObjectGeneral view status location =
   do
      (versioned,objectData) <- makeObjectData view status location
      setValue (objects view) location objectData
      return versioned

---
-- This deletes an object from the View.
-- NB.  It is very important to make sure that the object has first
-- been deleted from anything which references it by link (EG folders)
-- or you will get the program crashing when someone tries to follow the
-- link.  
--
-- Links provided by the LinkManager (and I don't know of any others) can
-- be deleted using LinkManager.deleteLinkedObject (which also calls
-- deleteLink).
deleteLink :: HasCodedValue x => View -> Link x -> IO ()
deleteLink view (Link location) = deleteFromRegistry (objects view) location

---
-- As with createObjectGeneral, create the versioned object and 
-- object data to put in the objects registry.  (All objects eventually
-- are created via this function.)
makeObjectData :: HasCodedValue x => View -> Status x -> Location 
   -> IO (Versioned x,ObjectData)
makeObjectData view (status :: Status x) location =
   do
      statusMVar <- newMVar status
      let versioned = Versioned {location = location,statusMVar = statusMVar}
      lastChange <- newIORef Nothing
      return (versioned,
         (PresentObject {
            thisVersioned = toDyn versioned,
            commitAct = commitVersioned view versioned,
            lastChange = lastChange
            })) 


updateObject :: HasCodedValue x => View -> x -> Versioned x -> IO ()
updateObject view x (versioned@Versioned{statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar

      newStatus <- case status of
         Empty -> return (Virgin x)
         Virgin _ -> return (Virgin x)
         UpToDate _ objectVersion ->
            do
               setLastChange view (location versioned)
               return (Dirty x objectVersion)
         Dirty _ objectVersion -> return (Dirty x objectVersion)
      putMVar statusMVar newStatus

dirtyObject :: HasCodedValue x => View -> Versioned x -> IO ()
dirtyObject view (versioned@Versioned {statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar

      newStatus <- case status of
         UpToDate x objectVersion ->
            do
               setLastChange view (location versioned)
               return (Dirty x objectVersion)
         _ -> return status

      putMVar statusMVar newStatus

readObject :: HasCodedValue x => View -> Versioned x -> IO x
readObject view (versioned@Versioned{statusMVar = statusMVar}) =
   do
      status <- readMVar statusMVar
      case status of
         Empty -> error "View.readObject on uninitialised object"
         Virgin x -> return x
         UpToDate x _ -> return x
         Dirty x _ -> return x

cloneObject 
   :: HasCodedValue x => View -> Versioned x -> View -> IO (Versioned x)
cloneObject oldView oldVersioned newView =
   do
      status <- readMVar (statusMVar oldVersioned)
      let
         loc = location oldVersioned
      newVersioned <- createObjectGeneral newView status loc
      -- Make sure lastChanged flag is replicated.
      case status of
         UpToDate _ _ ->
            do
               objectData <- getValue (objects oldView) loc
               (Just objectVersion) <- readIORef (lastChange objectData)
               updateLastChange newView loc objectVersion
         _ -> done  
      return newVersioned

-- ----------------------------------------------------------------------
-- Access to the lastChange
-- ----------------------------------------------------------------------

setLastChange :: View -> Location -> IO ()
setLastChange view location =
   do
      objectData <- getValue (objects view) location
      writeIORef (lastChange objectData) Nothing

updateLastChange :: View -> Location -> ObjectVersion -> IO ()
updateLastChange view location objectVersion =
   do
      objectData <- getValue (objects view) location
      writeIORef (lastChange objectData) (Just objectVersion)

getLastChange :: View -> Link object -> IO (Maybe ObjectVersion)
getLastChange view (Link location) =
   do
      objectData <- getValue (objects view) location
      readIORef (lastChange objectData)
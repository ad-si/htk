{- This module defines links, which are pointers to objects in the repository.
   Thus they can be part of other objects. -}
module Link(
   -- Links.
   -- A Link x is a pointer to an object of type x.  Links are made
   -- instances of HasCodedValue, which means they
   -- can themselves be stored in the repository, for example as attributes
   -- of other objects.
   Link,

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

   newEmptyObject, -- :: HasCodedValue x => View -> IO (Versioned x)
   -- This creates an object with no contents as a stop-gap so you can
   -- create a link to it, EG for constructing circular lists.
   -- WARNING - updateObject must be used to put in an actual value,
   -- before readObject is done on this Versioned value, or before
   -- commitView.

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
   ) where

import Concurrent

import Registry
import Dynamics
import AtomString(fromString)
import VariableSet(HasKey(..))

import VersionDB
import ViewType
import CodedValue
import CodedValueStore

-- ----------------------------------------------------------------------
-- Links
-- ----------------------------------------------------------------------

newtype Link x = Link Location

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
fetchLink (view@View{repository = repository,objects = objects}) 
      (Link location) =
   do
      transformValue objects location 
         (\ objectDataOpt ->
          do
            case objectDataOpt of
               Nothing -> error "View.fetchLink: Link to object not in view!!"
               Just (PresentObject versionedDyn _) ->
                  case fromDyn versionedDyn of
                     Just versioned -> return (objectDataOpt,versioned)
                     Nothing -> error "View.fetchLink - type error in link"
               Just (AbsentObject objectVersion) ->
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
                        PresentObject (toDyn versioned) 
                           (commitVersioned view versioned)),
                        versioned)
            )                 

readLink :: HasCodedValue x => View -> Link x -> IO x
readLink view link =
   do
      versioned <- fetchLink view link
      readObject view versioned 


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

commitVersioned :: HasCodedValue x => View -> Versioned x -> IO ObjectVersion
-- This is the action that gets done when we commit an object to
-- the repository.
commitVersioned (view@View{repository = repository})
      (Versioned{location = location,statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar
      let 
         commitX x parentVersionOpt =
            do
               xCodedValue <- doEncodeIO x view
               xObjectSource <- toObjectSource xCodedValue
               xObjectVersion <- commit repository xObjectSource location 
                  parentVersionOpt 
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
setTopLink view x = createObjectGeneral view (Virgin x) secondLocation

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
-- As with createObjectGeneral, create the versioned object and 
-- object data to put in the objects registry.  (All objects eventually
-- are created via this function.)
makeObjectData :: HasCodedValue x => View -> Status x -> Location 
   -> IO (Versioned x,ObjectData)
makeObjectData view (status :: Status x) location =
   do
      statusMVar <- newMVar status
      let versioned = Versioned {location = location,statusMVar = statusMVar}
      return (versioned,
         (PresentObject (toDyn versioned) (commitVersioned view versioned))) 


updateObject :: HasCodedValue x => View -> x -> Versioned x -> IO ()
updateObject view x (versioned@Versioned{statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar
      let
         newStatus = case status of
            Empty -> Virgin x
            Virgin _ -> Virgin x
            UpToDate _ objectVersion -> Dirty x objectVersion
            Dirty _ objectVersion -> Dirty x objectVersion
      putMVar statusMVar newStatus

dirtyObject :: HasCodedValue x => View -> Versioned x -> IO ()
dirtyObject view (versioned@Versioned {statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar
      let
         newStatus = case status of
            UpToDate x objectVersion ->
               Dirty x objectVersion
            _ -> status
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


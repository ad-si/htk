{- This module defines the fundamental structure of the (untyped) 
   objects in a repository. 

   We depend circularly on CodedValue.hs.  This module is compiled
   first and uses CodedValue.hi-boot.
-}
module View(
   View, -- A View represents a particular version being worked on
      -- by the client.
   newView, -- :: Repository -> IO View
      -- newView creates a wholly new view without respect to version.
   listViews, -- :: Repository -> IO [ObjectVersion]
      -- This lists the object versions for all currently checked-in
      -- (global) versions.
   getView, -- :: Repository -> ObjectVersion -> IO View
      -- This checks out a particular view.
   commitView, -- :: View -> IO ObjectVersion
      -- This commits all the objects in a particular view, returning
      -- a global version.
   getRepository, -- :: View -> Repository

   -- Links.
   -- A Link x is a pointer to an object of type x.  Links are made
   -- instances of HasCodedValue (in CodedValue.hs), which means they
   -- can themselves be stored in the repository, for example as attributes
   -- of other objects.
   Link,

   topLink, -- :: Link x
      -- This link points to the "top object".  This needs to be
      -- created 

   setTopLink, -- :: HasCodedValue x => View -> x -> IO (Versioned x)
      -- This initialises the top link to a particular value.  It should
      -- only be used once, in setting up the repository.

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
   -- been change.  This is needed, for example, when x points to a file,
   -- which has been updated on the side.
   readObject, -- :: HasCodedValue x => View -> Versioned x -> IO x
   -- get the current contents of the object.


   makeLink, -- :: HasCodedValue x => View -> Versioned x -> IO (Link x)
   -- makeLink is used to create a link to an object.
   fetchLink, -- :: HasCodedValue x => View -> Link x -> IO (Versioned x)
   -- look up a link to an object in the repository.
   ) where

import Directory

import Concurrent

import Registry
import Dynamics
import AtomString(fromString)
import UniqueFile
import FileNames
import WBFiles

import VersionDB
import ViewType
import CodedValue
import CodedValueStore

-- ----------------------------------------------------------------------
-- Views
-- (The View datatype is imported from ViewType)
--
-- Conventions.  The index to the view is stored in location
-- firstLocation.  The top link is secondLocation.
-- ----------------------------------------------------------------------

newView :: Repository -> IO View
-- newView creates a wholly new view without respect to version.
-- This should be used for initialising the repository, and never again.
newView repository =
   do
      objects <- newRegistry
      parentMVar <- newMVar Nothing
      displayTypes <- newRegistry
      objectTypes <- newRegistry

      return (View {
         repository = repository,
         objects = objects,
         parentMVar = parentMVar,
         displayTypes = displayTypes,
         objectTypes = objectTypes
         })

listViews :: Repository -> IO [ObjectVersion]
listViews repository = listVersions repository firstLocation

getView :: Repository -> ObjectVersion -> IO View
getView repository objectVersion =
   do
      objectsString <- retrieveString repository firstLocation objectVersion
      let
         objectsCodedValue = fromString objectsString
         phantomView = error "CodedValue for links and version needs View!"
         -- It shouldn't, it goes via HasCodedValuePure.  But we need
         -- HasCodedValue for technical reasons since lists and tuples
         -- can't be defined for HasPureCodedValue to avoid a nasty instance
         -- overlap.
      (objectsList :: [(Location,ObjectVersion)]) 
         <- doDecodeIO objectsCodedValue phantomView
      objects <- newRegistry
      sequence_ (map
         (\ (location,objectVersion) -> 
            setValue objects location (AbsentObject objectVersion)
            )
         objectsList
         )
      parentMVar <- newMVar (Just objectVersion)
      
      return (View {
         repository = repository,
         objects = objects,
         parentMVar = parentMVar
         })

commitView :: View -> IO ObjectVersion
commitView (View {repository = repository,objects = objects,
      parentMVar = parentMVar}) =
   -- NB - this ought to lock against updates, but at the moment doesn't.
   -- But we do lock against other commits using parentMVar.
   do
      parentOpt <- takeMVar parentMVar
      locations <- listKeys objects
      (objectsList :: [(Location,ObjectVersion)]) <-
         mapM (\ location ->
            do
               objectData <- getValue objects location
               objectVersion <- case objectData of 
                  AbsentObject objectVersion -> return objectVersion
                  PresentObject _ commitAction -> commitAction
               return (location,objectVersion)
            )
            locations
      let
         phantomView = error "CodedValue for links and version needs View (2)!"
      objectsCodedValue <- doEncodeIO objectsList phantomView
      objectsObjectSource <- toObjectSource objectsCodedValue 
      newObjectVersion <- commit repository objectsObjectSource firstLocation 
         parentOpt
      putMVar parentMVar (Just newObjectVersion)
      return newObjectVersion

getRepository :: View -> Repository
getRepository view = repository view

-- ----------------------------------------------------------------------
-- Links
-- ----------------------------------------------------------------------

newtype Link x = Link Location

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

-- ----------------------------------------------------------------------
-- Versioned
-- ----------------------------------------------------------------------

data Versioned x = Versioned {
   location :: Location, -- Location in the view.
   statusMVar :: MVar (Status x)
   }

-- Make Versioned objects typeable (if x is).
versioned_tyCon = mkTyCon "View" "Versioned"

instance HasTyCon1 Versioned where
   tyCon1 _ = versioned_tyCon

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

   
      -- This link points to the "top object".  This needs to be
      -- created 

setTopLink :: HasCodedValue x => View -> x -> IO (Versioned x)
setTopLink view x = createObjectGeneral view (Virgin x) secondLocation

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
      statusMVar <- newMVar status
      let versioned = Versioned {location = location,statusMVar = statusMVar}
      setValue (objects view) location 
         (PresentObject (toDyn versioned) (commitVersioned view versioned))
      return versioned

updateObject :: HasCodedValue x => View -> x -> Versioned x -> IO ()
updateObject view x (Versioned{statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar
      let
         newStatus = case status of
            Empty -> Virgin x
            Virgin _ -> Virgin x
            UpToDate _ objectVersion -> Dirty x objectVersion
            Dirty _ objectVersion -> Dirty x objectVersion
      putMVar statusMVar status

dirtyObject :: HasCodedValue x => View -> Versioned x -> IO ()
dirtyObject view (Versioned {statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar
      let
         newStatus = case status of
            UpToDate x objectVersion -> Dirty x objectVersion
            _ -> status
      putMVar statusMVar status

readObject :: HasCodedValue x => View -> Versioned x -> IO x
readObject view (Versioned{statusMVar = statusMVar}) =
   do
      status <- readMVar statusMVar
      case status of
         Empty -> error "View.readObject on uninitialised object"
         Virgin x -> return x
         UpToDate x _ -> return x
         Dirty x _ -> return x




   
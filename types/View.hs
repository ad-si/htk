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
   Version, 
      -- type of the version of a view
   getView, -- :: Repository -> Version -> IO View
      -- This checks out a particular view.
   commitView, -- :: View -> IO Version
      -- This commits all the objects in a particular view, returning
      -- a global version.
   synchronizeView, -- :: View -> IO b -> IO b
      -- Perform some action during which no commit should take place.
   createViewObject, 
      -- :: (HasCodedValue object)
      -- => View -> (Link object -> IO (object,extra))
      -- -> IO extra
      -- Function for creating an object which requires its own link.  

   parentVersions, -- :: View -> IO [Version]
      -- returns the parent versions of this view.
   ) where

import Directory

import Concurrent


import Debug(debug)
import Registry
import Dynamics
import AtomString(fromString)
import Object
import FileSystem
import CopyFile
import Sources
import Broadcaster
import Delayer

import VSem

import Destructible

import VersionDB
import ViewType
import CodedValue
import CodedValueStore
import DisplayTypes
import ObjectTypes
import Link
import GlobalRegistry


-- ----------------------------------------------------------------------
-- The Version type
-- ----------------------------------------------------------------------

type Version = ObjectVersion

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
      parentsMVar <- newMVar []
      viewIdObj <- newObject
      fileSystem <- newFileSystem
      titleSource <- newSimpleBroadcaster ""
      commitLock <- newVSem
      delayer <- newDelayer

      return (View {
         viewId = ViewId viewIdObj,
         repository = repository,
         objects = objects,
         parentsMVar = parentsMVar,
         titleSource = titleSource,
         fileSystem = fileSystem,
         commitLock = commitLock,
         delayer = delayer
         })

listViews :: Repository -> IO [Version]
listViews repository = listVersions repository firstLocation

getView :: Repository -> Version -> IO View
getView repository objectVersion =
   do
      objectId <- newObject
      let viewId = ViewId objectId
      viewString <- retrieveString repository firstLocation objectVersion
      let
         viewCodedValue = fromString viewString

         phantomView = error "CodedValue for view needs View!"
         -- It shouldn't, it goes via HasCodedValuePure.  But we need
         -- HasCodedValue for technical reasons since lists and tuples
         -- can't be defined for HasPureCodedValue to avoid a nasty instance
         -- overlap.
      (ViewData {
         objectsData = objectsData,
         displayTypesData = displayTypesData,
         objectTypesData = objectTypesData
         }) <- doDecodeIO viewCodedValue phantomView

      -- Convert lists to registries.  objectsData requires special handling 
      -- because the registry is a LockedRegistry and doesn't have a direct 
      -- function.
      objects <- newRegistry
      sequence_ (map
         (\ (location,objectVersion) -> 
            setValue objects location (AbsentObject objectVersion)
            )
         objectsData
         )

      parentsMVar <- newMVar [objectVersion]
      fileSystem <- newFileSystem
      titleSource <- newSimpleBroadcaster ""
      commitLock <- newVSem
      delayer <- newDelayer
      let
         view = View {
            viewId = viewId,
            repository = repository,
            objects = objects,
            titleSource = titleSource,
            parentsMVar = parentsMVar,
            fileSystem = fileSystem,
            commitLock = commitLock,
            delayer = delayer
            }

      importDisplayTypes displayTypesData view
      importObjectTypes objectTypesData view
      return view

commitView :: View -> IO Version
commitView (view @ View {repository = repository,objects = objects,
      parentsMVar = parentsMVar,commitLock = commitLock}) =
   synchronizeGlobal commitLock (
      do
         parents <- takeMVar parentsMVar

         displayTypesData <- exportDisplayTypes view

         objectTypesData <- exportObjectTypes view

         locations <- listKeys objects
         (objectsData :: [(Location,Version)]) <-
            mapM
               (\ location ->
                  do
                     objectsData <- getValue objects location
                     objectVersion <- case objectsData of 
                        AbsentObject objectVersion -> return objectVersion
                        PresentObject _ commitAction -> commitAction
                     return (location,objectVersion)
                  )
                  locations
               
         let
            viewData =
               ViewData {
                  objectsData = objectsData,
                  displayTypesData = displayTypesData,
                  objectTypesData = objectTypesData
                  }

            phantomView = error "CodedValue for view needs View (2)!"

         viewCodedValue <- doEncodeIO viewData phantomView
         viewObjectSource <- toObjectSource viewCodedValue 
         newVersion <- commit repository viewObjectSource firstLocation 
            (case parents of 
               [] -> Nothing
               parent : _ -> Just parent
               )

         putMVar parentsMVar [newVersion]
         return newVersion
      )
---
-- returns the current parent version of the view.
parentVersions :: View -> IO [Version]
parentVersions view = readMVar (parentsMVar view)

-- ----------------------------------------------------------------------
-- Format of view information in the top file
-- ----------------------------------------------------------------------

-- ViewData is the information needed to construct a view
-- which we store in the top file of a version.
data ViewData = ViewData {
   objectsData :: [(Location,ObjectVersion)],
   displayTypesData :: CodedValue,
   objectTypesData :: CodedValue
   }

viewData_tyRep = mkTyRep "View" "ViewData"
instance HasTyRep ViewData where
   tyRep _ = viewData_tyRep

-- Here's the real primitive type
type Tuple = ([(Location,ObjectVersion)],CodedValue,CodedValue)

mkTuple :: ViewData -> Tuple
mkTuple (ViewData {objectsData = objectsData,displayTypesData = displayTypesData,
   objectTypesData = objectTypesData}) =
      (objectsData,displayTypesData,objectTypesData)

unmkTuple :: Tuple -> ViewData
unmkTuple (objectsData,displayTypesData,objectTypesData) =
   ViewData {objectsData = objectsData,displayTypesData = displayTypesData,
   objectTypesData = objectTypesData}

instance HasCodedValue ViewData where
   encodeIO = mapEncodeIO mkTuple 
   decodeIO = mapDecodeIO unmkTuple

-- ---------------------------------------------------------------------
-- synchronizeView
-- ---------------------------------------------------------------------

-- Perform some action during which no commit should take place.
synchronizeView :: View -> IO b -> IO b
synchronizeView view action = synchronizeLocal (commitLock view) action

-- ----------------------------------------------------------------------
-- createViewObject
-- ----------------------------------------------------------------------

---
-- Function for creating an object which requires its own link. 
-- The function provided returns Nothing if there was an error and the
-- object should not, after all, be inserted. 
createViewObject :: (HasCodedValue object) 
   => View -> (Link object -> IO (Maybe object,extra))
   -> IO extra
createViewObject view getObject =
   synchronizeView view (
      do
         versioned <- newEmptyObject view
         link <- makeLink view versioned
         (objectOpt,extra) <- getObject link
         case objectOpt of
            Just object -> updateObject view object versioned
            Nothing -> deleteLink view link
         return extra
      )

-- ----------------------------------------------------------------------
-- Instance of Destroyable
-- ----------------------------------------------------------------------

instance Object View where
   objectID (View {viewId = ViewId oID}) = oID

instance Destroyable View where
   destroy view =
      do
         allObjectTypeTypes <- getAllObjectTypeTypes
         mapM_ 
            (\ (WrappedObjectTypeTypeData objectType) ->
               deleteViewFromGlobalRegistry 
                  (objectTypeGlobalRegistry objectType)
                  view
               )
            allObjectTypeTypes 

         allDisplayTypes <- getAllDisplayTypeTypes
         mapM_
            (\ (WrappedDisplayType displayType) ->
               deleteViewFromGlobalRegistry
                  (displayTypeGlobalRegistry displayType)
                  view
               )
            allDisplayTypes
 
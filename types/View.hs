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
   ) where

import Directory

import Concurrent

import Registry
import Dynamics
import AtomString(fromString)

import VersionDB
import ViewType
import CodedValue
import CodedValueStore
import DisplayTypes
import ObjectTypes

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

      -- Convert lists to registries.  objectsData requires special handling because
      -- the registry is a LockedRegistry and doesn't have a direct function.
      objects <- newRegistry
      sequence_ (map
         (\ (location,objectVersion) -> 
            setValue objects location (AbsentObject objectVersion)
            )
         objectsData
         )

      displayTypes <- listToNewRegistry displayTypesData
      objectTypes <- listToNewRegistry objectTypesData

      parentMVar <- newMVar (Just objectVersion)
      
      return (View {
         repository = repository,
         objects = objects,
         parentMVar = parentMVar,
         displayTypes = displayTypes,
         objectTypes = objectTypes
         })

commitView :: View -> IO ObjectVersion
commitView (View {repository = repository,objects = objects,
      parentMVar = parentMVar,displayTypes = displayTypes,objectTypes = objectTypes}) =
   -- NB - this ought to lock against updates, but at the moment doesn't.
   -- But we do lock against other commits using parentMVar.
   do
      parentOpt <- takeMVar parentMVar
      locations <- listKeys objects
      (objectsData :: [(Location,ObjectVersion)]) <-
         mapM (\ location ->
            do
               objectsData <- getValue objects location
               objectVersion <- case objectsData of 
                  AbsentObject objectVersion -> return objectVersion
                  PresentObject _ commitAction -> commitAction
               return (location,objectVersion)
            )
            locations
      displayTypesData <- listRegistryContents displayTypes
      objectTypesData <- listRegistryContents objectTypes
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
      newObjectVersion <- commit repository viewObjectSource firstLocation 
         parentOpt
      putMVar parentMVar (Just newObjectVersion)
      return newObjectVersion


-- ----------------------------------------------------------------------
-- Format of view information in the top file
-- ----------------------------------------------------------------------

-- ViewData is the information needed to construct a view
-- which we store in the top file of a version.
data ViewData = ViewData {
   objectsData :: [(Location,ObjectVersion)],
   displayTypesData :: [(String,WrappedDisplayType)],
   objectTypesData :: [(String,WrappedObjectType)]
   }

viewData_tyCon = mkTyCon "View" "ViewData"
instance HasTyCon ViewData where
   tyCon _ = viewData_tyCon

-- Here's the real primitive type
type Tuple = ([(Location,ObjectVersion)],[(String,WrappedDisplayType)],[(String,WrappedObjectType)])

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


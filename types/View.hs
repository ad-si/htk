-- | This module defines the fundamental structure of the (untyped) 
-- objects in a repository. 
-- 
-- We depend circularly on CodedValue.hs.  This module is compiled
-- first and uses CodedValue.hi-boot.
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
   getView, -- :: Repository -> VersionSimpleGraph -> Version -> IO View
      -- This checks out a particular view.
   commitView, -- :: View -> IO Version
      -- This commits all the objects in a particular view, returning
      -- a global version.
   commitView1, -- :: CommitInfo -> ObjectVersion -> View -> IO Version
      -- Slightly more general version where the object version is
      -- pre-allocated.
   CommitInfo(..),
      -- describes information preserved on commit.

   synchronizeView, -- :: View -> IO b -> IO b
      -- Perform some action during which no commit should take place.
   createViewObject, 
      -- :: (HasCodedValue object)
      -- => View -> (Link object -> IO (object,extra))
      -- -> IO extra
      -- Function for creating an object which requires its own link.  

   parentVersions, -- :: View -> IO [Version]
      -- returns the parent versions of this view.

   setUserInfo, -- :: View -> UserInfo -> IO VersionInfo
      -- set the user info for the view, returning the new VersionInfo.

   readVersionInfo, -- :: View -> IO VersionInfo
      -- get the VersionInfo for this view.
   ) where

import Directory
import Maybe

import Data.IORef
import Control.Concurrent.MVar


import Debug(debug)
import Registry
import Dynamics
import AtomString(fromString)
import Object
import CopyFile
import Sources
import Broadcaster
import Delayer
import Store

import VSem

import Destructible

import VersionInfo

import VersionDB
import VersionGraphClient
import ViewType
import CodedValue
import DisplayTypes
import ObjectTypes
import Link
import GlobalRegistry


-- ----------------------------------------------------------------------
-- Datatypes
-- ----------------------------------------------------------------------

type Version = ObjectVersion

data CommitInfo = 
      OnlyVersionNumber
   |  OnlyUserInfo
   |  AllVersionInfo

-- ----------------------------------------------------------------------
-- Views
-- (The View datatype is imported from ViewType)
--
-- Conventions.  The index to the view is stored in location
-- specialLocation1.  The top link is specialLocation2.
-- ----------------------------------------------------------------------

newView :: Repository -> IO View
-- newView creates a wholly new view without respect to version.
-- This should be used for initialising the repository, and never again.
newView repository =
   do
      objects <- newRegistry
      viewInfoBroadcaster <- newSimpleBroadcaster topVersionInfo
      viewIdObj <- newObject
      commitLock <- newVSem
      delayer <- newDelayer
      committingVersion <- newMVar Nothing
      importsState <- newStore

      return (View {
         viewId = ViewId viewIdObj,
         repository = repository,
         objects = objects,
         viewInfoBroadcaster = viewInfoBroadcaster,
         commitLock = commitLock,
         delayer = delayer,
         committingVersion = committingVersion,
         versionGraph1 = error 
            "Attempt to read version graph during initialisation",
         importsState = importsState
         })

listViews :: Repository -> IO [Version]
listViews repository = listVersions repository

getView :: Repository -> VersionSimpleGraph -> Version -> IO View
getView repository versionGraph objectVersion =
   do
      objectId <- newObject
      let viewId = ViewId objectId
      viewString <- retrieveString repository specialLocation1 objectVersion
      let
         viewCodedValue = fromString viewString

         phantomView = error "CodedValue for view needs View!"
         -- It shouldn't, it goes via HasCodedValuePure.  But we need
         -- HasCodedValue for technical reasons since lists and tuples
         -- can't be defined for HasPureCodedValue to avoid a nasty instance
         -- overlap.
      (ViewData {
         displayTypesData = displayTypesData,
         objectTypesData = objectTypesData
         }) <- doDecodeIO viewCodedValue phantomView

      viewInfo0 <- getVersionInfo versionGraph (versionToNode objectVersion)
      let
         user0 = user viewInfo0

         -- set appropriate fields of viewInfo.
         user1 = user0 {parents = [objectVersion]}
         viewInfo1 = viewInfo0 {user = user1}
      
      objects <- newRegistry
      viewInfoBroadcaster <- newSimpleBroadcaster viewInfo1
      commitLock <- newVSem
      delayer <- newDelayer
      committingVersion <- newMVar Nothing
      importsState <- newStore
      let
         view = View {
            viewId = viewId,
            repository = repository,
            objects = objects,
            viewInfoBroadcaster = viewInfoBroadcaster,
            commitLock = commitLock,
            delayer = delayer,
            committingVersion = committingVersion,
            versionGraph1 = versionGraph,
            importsState = importsState
            }

      importDisplayTypes displayTypesData view
      importObjectTypes objectTypesData view
      return view

commitView :: View -> IO Version
commitView view =
   do
      newVersion1 <- newVersion (repository view)
      commitView1 OnlyUserInfo newVersion1 view

commitView1 :: CommitInfo -> ObjectVersion -> View -> IO Version
commitView1 commitInfo newVersion1 
   (view @ View {repository = repository,objects = objects,
      commitLock = commitLock}) =
   synchronizeGlobal commitLock (
      do
         swapMVar (committingVersion view) (Just newVersion1)

         displayTypesData <- exportDisplayTypes view

         objectTypesData <- exportObjectTypes view

         locations <- listKeys objects

         let
            mkCommitChange :: ObjectData -> ObjectVersion 
               -> IO (Maybe CommitChange)
            mkCommitChange (PresentObject {mkObjectSource = mkObjectSource})
               objectVersion = mkObjectSource objectVersion
            mkCommitChange (ClonedObject {sourceLocation = sourceLocation,
               sourceVersion = sourceVersion}) _
                  = return (Just (Right (sourceLocation,sourceVersion)))

         (objectsData0 :: [Maybe (Location,CommitChange)]) <-
            -- compute the data for objects to commit.
            mapM
               (\ location ->
                  do
                     objectData <- getValue objects location
                     objectSourceOpt <- mkCommitChange objectData newVersion1
                     return (fmap
                        (\ objectSource -> (location,objectSource))
                        objectSourceOpt
                        )
                  )
               locations
         let
            objectsData1 :: [(Location,CommitChange)]
            objectsData1 = catMaybes objectsData0

         viewInfo0 <- readContents (viewInfoBroadcaster view)
               
         let
            viewData =
               ViewData {
                  displayTypesData = displayTypesData,
                  objectTypesData = objectTypesData
                  }

            phantomView = error "CodedValue for view needs View (2)!"

         viewCodedValue <- doEncodeIO viewData phantomView
         viewObjectSource <- importICStringLen viewCodedValue 

         let
            objectsData2 :: [(Location,CommitChange)]
            objectsData2 = (specialLocation1,Left viewObjectSource) 
               : objectsData1

            user0 = user viewInfo0
            user1 = user0 {version = newVersion1}

            versionInformation = 
              case commitInfo of
                 OnlyVersionNumber -> Version1 newVersion1
                 OnlyUserInfo -> UserInfo1 user1
                 AllVersionInfo -> VersionInfo1 (viewInfo0 {user = user1})

         commit repository versionInformation [] objectsData2

         let
            user2 = user1 {parents = [newVersion1]}
            viewInfo1 = viewInfo0 {user = user2}

         broadcast (viewInfoBroadcaster view) viewInfo1

         swapMVar (committingVersion view) Nothing
         return newVersion1
      )

-- ----------------------------------------------------------------------
-- Format of view information in the top file
-- ----------------------------------------------------------------------

-- ViewData is the information needed to construct a view
-- which we store in the top file of a version.
data ViewData = ViewData {
   displayTypesData :: CodedValue,
   objectTypesData :: CodedValue
   } deriving (Typeable)

-- Here's the real primitive type
type Tuple = (CodedValue,CodedValue)

mkTuple :: ViewData -> Tuple
mkTuple (ViewData {
   displayTypesData = displayTypesData,
   objectTypesData = objectTypesData}) =
      (displayTypesData,objectTypesData)

unmkTuple :: Tuple -> ViewData
unmkTuple (displayTypesData,objectTypesData) =
   ViewData {
      displayTypesData = displayTypesData,objectTypesData = objectTypesData}

instance HasBinary ViewData CodingMonad where
   writeBin = mapWrite mkTuple
   readBin = mapRead unmkTuple

-- ---------------------------------------------------------------------
-- synchronizeView
-- ---------------------------------------------------------------------

-- Perform some action during which no commit should take place.
synchronizeView :: View -> IO b -> IO b
synchronizeView view action = synchronizeLocal (commitLock view) action

-- ---------------------------------------------------------------------
-- Accessing the UserInfo for a view.
-- ---------------------------------------------------------------------

setUserInfo :: View -> UserInfo -> IO ()
setUserInfo view userInfo =
   applySimpleUpdate (viewInfoBroadcaster view)
      (\ versionInfo0 ->
         let
            user0 = user versionInfo0
            user1 = user0 {
               label = label userInfo,
               contents = contents userInfo,
               private = private userInfo
               }
            versionInfo1 = versionInfo0 {user = user1}
         in
            versionInfo1
  
         )        
   
readVersionInfo :: View -> IO VersionInfo
readVersionInfo view = readContents (viewInfoBroadcaster view)

-- ----------------------------------------------------------------------
-- createViewObject
-- ----------------------------------------------------------------------

-- | Function for creating an object which requires its own link. 
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
 

-- | ViewType defines the actual type of a view.  Note that we don't actually
-- do anything with it in this file; the real work is done in View.
-- However, defining the type here means CodedValue can import it,
-- and View can import CodedValue, without a circularity.
--
-- However circularities can't be entirely avoided, see RECURSION.
module Types.ViewType(
   View(..),
   ObjectData(..),

   getRepository, -- :: View -> Repository

   ViewId(..),

   getViewTitleSource, -- :: View -> Source String

   parentVersions, -- :: View -> IO [ObjectVersion]
   getParentVersion, -- :: View -> Maybe ObjectVersion
   getParentLocationInView, -- :: View -> Location -> IO (Maybe Location)

   ) where

import Control.Concurrent

import Util.QuickReadShow
import Util.Object
import Util.Dynamics
import Util.Registry
import Util.Broadcaster
import Util.Sources
import Util.Delayer
import Util.Store
import Util.ExtendedPrelude(mapEq,mapOrd)

import Util.VSem

import Imports.Imports(ImportsState)

import SimpleDB.VersionInfo
import Types.VersionDB
import {-# SOURCE #-} Types.LinkManager
import {-# SOURCE #-} Types.VersionGraphClient

data View = View {
   viewId :: ViewId,
   repository :: Repository,
   objects :: LockedRegistry Location ObjectData,
      -- ^ Dictionary of all objects.
   parentChanges :: Registry Location Location,
      -- ^ Contains parent information not yet communicated to the server.
      -- This only needs to be a Registry because transformValue is not
      -- used.

   viewInfoBroadcaster :: SimpleBroadcaster VersionInfo,
      -- ^ Current extra data for his view.
      -- The server part is usually junk, and discarded on commit.
      -- However we keep it, as it needs to be preserved on transmitting
      -- a view from one repository to another.

   -- locally locked while some update operations are going on;
   -- globally locked during commits.
   commitLock :: VSem,

   -- Blocked when complex updates are going on.
   delayer :: Delayer,

   -- If set, this means we are in the middle of a commit, and the
   -- view is going to have this version.
   committingVersion :: MVar (Maybe ObjectVersion),

   -- This is a version graph for the repository.
   graphClient1 :: VersionGraphClient,

   -- This is an imports state, or will be when it's initialised.
   importsState :: Store (ImportsState LinkedObject)
   }

data ObjectData =
      PresentObject {
         -- Object is checked out.
         thisVersioned :: Dyn,
            -- Versioned x for it.
         mkObjectSource :: ObjectVersion -> IO (Maybe CommitChange)
            -- action that constructs the information to commit for the object.
            -- (For the meaning of the Either, see VersionDB.commit.)
            -- The supplied ObjectVersion is that belonging to the
            -- containing view.

            --
            -- If it returns Nothing, that means the object does not
            -- need to be updated, as this version is up-to-date.
         }
   |  ClonedObject {
         sourceLocation :: Location,
         sourceVersion :: ObjectVersion
         }

getRepository :: View -> Repository
getRepository view = repository view

newtype ViewId = ViewId ObjectID deriving (Eq,Ord)

instance QuickShow ViewId where
   quickShow = WrapShow (\ (ViewId oId) -> oId)

instance Show ViewId where
   showsPrec = qShow

instance Eq View where
   (==) = mapEq viewId

instance Ord View where
   compare = mapOrd viewId

-- -----------------------------------------------------------------
-- Extracting the parents
-- -----------------------------------------------------------------

parentVersions :: View -> IO [ObjectVersion]
parentVersions view =
   do
      versionInfo <- readContents (viewInfoBroadcaster view)
      return (parents (user versionInfo))

-- getParentVersion retrieves the version number sent to the server
-- on requesting items not already in the view.
getParentVersion :: View -> IO (Maybe ObjectVersion)
getParentVersion view =
   do
      parents <- parentVersions view
      return (case parents of
         parent : _ -> Just parent
         [] -> Nothing
         )

-- -----------------------------------------------------------------
-- Function for extracting the title of a View as a SimpleSource
-- -----------------------------------------------------------------

getViewTitleSource :: View -> SimpleSource String
getViewTitleSource view =
   fmap (label . user) (toSimpleSource . viewInfoBroadcaster $ view)

-- -----------------------------------------------------------------
-- Get the parent location of a location in a view
-- -----------------------------------------------------------------

getParentLocationInView :: View -> Location -> IO (Maybe Location)
getParentLocationInView view thisLocation =
   do
      parentLocationOpt <- getValueOpt (parentChanges view) thisLocation
      case parentLocationOpt of
         Just parentLocation -> return parentLocationOpt
            -- object has been moved since last commit
         Nothing ->
            do
               versionOpt <- getParentVersion view
               case versionOpt of
                  Nothing -> return Nothing
                  Just version ->
                     getParentLocation (repository view) version thisLocation

-- -----------------------------------------------------------------
-- Instance of HasDelayer
-- -----------------------------------------------------------------

instance HasDelayer View where
   toDelayer view = delayer view

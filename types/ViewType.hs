{- ViewType defines the actual type of a view.  Note that we don't actually
   do anything with it in this file; the real work is done in View.
   However, defining the type here means CodedValue can import it,
   and View can import CodedValue, without a circularity. 

   However circularities can't be entirely avoided, see RECURSION. -}
module ViewType(
   View(..),
   ObjectData(..),

   getRepository, -- :: View -> Repository

   ViewId(..),

   getViewTitleSource, -- :: View -> Source String

   getParentVersion, -- :: View -> Maybe ObjectVersion

   ) where

import Control.Concurrent

import Data.IORef

import QuickReadShow
import Object
import Dynamics
import Registry
import UniqueFile
import FileSystem
import Broadcaster
import Sources
import Delayer

import VSem

import VersionDB

data View = View {
   viewId :: ViewId,
   repository :: Repository,
   objects :: LockedRegistry Location ObjectData,

   parentsMVar :: MVar [ObjectVersion],
   -- parents of this view.  (None for the first version, multiple for
   -- merged versions.)
   --
   -- The first element of this list, if any, is special in that it is
   -- used as the version sent to the server when requesting links not
   -- already in the view, by the getParentVersion function.
 
   titleSource :: SimpleBroadcaster String, -- current title of this view.

   -- Contains "real" copies of files for the benefit of tools
   fileSystem :: FileSystem,

   -- locally locked while some update operations are going on;
   -- globally locked during commits.
   commitLock :: VSem,

   -- Blocked when complex updates are going on.
   delayer :: Delayer,

   -- If set, this means we are in the middle of a commit, and the
   -- view is going to have this version.
   committingVersion :: MVar (Maybe ObjectVersion)
   }

data ObjectData =
      PresentObject {
         -- Object is checked out.
         thisVersioned :: Dyn,
            -- Versioned x for it.
         mkObjectSource :: ObjectVersion -> IO (Maybe ObjectSource)
            -- action that constructs the ObjectSource for the object.
            -- The supplied ObjectVersion is that belonging to the
            -- containing view.
            --
            -- If it returns Nothing, that means the object does not
            -- need to be updated, as this version is up-to-date.
         }

getRepository :: View -> Repository
getRepository view = repository view

newtype ViewId = ViewId ObjectID deriving (Eq,Ord)

instance QuickShow ViewId where
   quickShow = WrapShow (\ (ViewId oId) -> oId)

-- -----------------------------------------------------------------
-- getParentVersion
-- -----------------------------------------------------------------

-- getParentVersion retrieves the version number sent to the server
-- on requesting items not already in the view.
getParentVersion :: View -> IO (Maybe ObjectVersion)
getParentVersion view =
   do
      parents <- readMVar (parentsMVar view)
      return (case parents of
         parent : _ -> Just parent
         [] -> Nothing
         )

-- -----------------------------------------------------------------
-- Function for extracting the title of a View as a SimpleSource
-- -----------------------------------------------------------------

getViewTitleSource :: View -> SimpleSource String
getViewTitleSource view = toSimpleSource (titleSource view)


-- -----------------------------------------------------------------
-- Instance of HasDelayer
-- -----------------------------------------------------------------

instance HasDelayer View where
   toDelayer view = delayer view
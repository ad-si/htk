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
         commitAct :: ObjectVersion -> IO ObjectVersion,
            -- commit action returning the new object version
            -- The supplied ObjectVersion is that belonging to the
            -- containing view (or top link).
         lastChange :: IORef (Maybe ObjectVersion)
            -- ObjectVersion for top link of view in which this object 
            -- was last changed.  If the object was changed in this view, 
            -- lastChange is Nothing.
         }
   |  AbsentObject {
         -- Object is not checked out.
         thisObjectVersion :: ObjectVersion,
            -- current version of object.
         lastChange :: IORef (Maybe ObjectVersion)
            -- see comments for PresentObject
         }

getRepository :: View -> Repository
getRepository view = repository view

newtype ViewId = ViewId ObjectID deriving (Eq,Ord)

instance QuickShow ViewId where
   quickShow = WrapShow (\ (ViewId oId) -> oId)

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
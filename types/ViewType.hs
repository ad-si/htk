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

import Concurrent

import QuickReadShow
import Object
import Dynamics
import Registry
import UniqueFile
import FileSystem
import Source

import BSem

import VersionDB

data View = View {
   viewId :: ViewId,
   repository :: Repository,
   objects :: LockedRegistry Location ObjectData,
   parentMVar :: MVar (Maybe ObjectVersion),

   titleSource :: SimpleSource String, -- current title of this view.

   -- Contains "real" copies of files for the benefit of tools
   fileSystem :: FileSystem
   }

data ObjectData =
      PresentObject Dyn (IO ObjectVersion)
         -- Object is checked out.  The Dyn will be a "Versioned x" for it,
         -- the action commits the latest version, and returns the new
         -- object version
   |  AbsentObject ObjectVersion
         -- Object is not checked out, and this is the current version. 

getRepository :: View -> Repository
getRepository view = repository view

newtype ViewId = ViewId ObjectID deriving (Eq,Ord)

instance QuickShow ViewId where
   quickShow = WrapShow (\ (ViewId oId) -> oId)

-- -----------------------------------------------------------------
-- Function for extracting the title of a View as a Source
-- -----------------------------------------------------------------

getViewTitleSource :: View -> Source String
getViewTitleSource view = mkSource (titleSource view)
{- ViewType defines the actual type of a view.  Note that we don't actually
   do anything with it in this file; the real work is done in View.
   However, defining the type here means CodedValue can import it,
   and View can import CodedValue, without a circularity. -}
module ViewType(
   View(..),
   ObjectData(..),
   ) where

import Concurrent

import Dynamics
import Registry
import UniqueFile

import BSem

import VersionDB

data View = View {
   repository :: Repository,
   objects :: LockedRegistry Location ObjectData,
   parentMVar :: MVar (Maybe ObjectVersion)
   }

data ObjectData =
      PresentObject Dyn (IO ObjectVersion)
         -- Object is checked out.  The Dyn will be a "Versioned x" for it,
         -- the action commits the latest version, and returns the new
         -- object version
   |  AbsentObject ObjectVersion
         -- Object is not checked out, and this is the current version. 
{- ViewType defines the actual type of a view.  Note that we don't actually
   do anything with it in this file; the real work is done in View.
   However, defining the type here means CodedValue can import it,
   and View can import CodedValue, without a circularity. 

   However circularities can't be entirely avoided, see RECURSION. -}
module ViewType(
   View(..),
   ObjectData(..),

   getRepository, -- :: View -> Repository
   ) where

import Concurrent

import Dynamics
import Registry
import UniqueFile

import BSem

import VersionDB

-- Recursive imports!
import {-# SOURCE #-} DisplayTypes
import {-# SOURCE #-} ObjectTypes

data View = View {
   repository :: Repository,
   objects :: LockedRegistry Location ObjectData,
   parentMVar :: MVar (Maybe ObjectVersion),
   
   displayTypes :: Registry String WrappedDisplayType,
   objectTypes :: Registry String WrappedObjectType
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


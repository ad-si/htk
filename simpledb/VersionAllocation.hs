-- | This module handles the allocation of 'ObjectVersion's.
module VersionAllocation(
   initVersions,
   allocVersion,
   useVersion,
   reuseVersion,
   flushVersion,
   ) where

import GHC.Weak
import Data.FiniteMap
import Data.IORef

import Data.IORef

import Computation

import PasswordFile(User)

import ServerErrors
import VersionInfo
import SimpleDBTypes
import BDBOps
import BDBExtras

initVersions :: BDB -> IO (IORef ObjectVersion)
initVersions bdb =
   do
      objectVersion <- getObject bdb 0
      newIORef objectVersion

-- | Allocate a new version, and add it to 'openVersions'.
allocVersion :: SimpleDB -> User -> IO ObjectVersion
allocVersion simpleDB user =
   do
      newVersion <- atomicModifyIORef (nextVersion simpleDB) 
         (\ version -> (succ version,version))
      reuseVersion simpleDB user newVersion
      return newVersion

-- | Use a version, taking it out of 'openVersions', or
-- provoke an error if it is not there.
useVersion :: SimpleDB -> User -> ObjectVersion -> IO ()
useVersion simpleDB user version =
   do
      userWeakRefOpt <- atomicModifyIORef (openVersions simpleDB)
         (\ fm0 -> (delFromFM fm0 version,lookupFM fm0 version))
      let
         notAllowed = throwError AccessError
            "Attempt to commit version which is not allocated"

      case userWeakRefOpt of
         Nothing -> notAllowed
         Just userWeakRef ->
            do
               userOpt <- deRefWeak userWeakRef
               case userOpt of
                  Nothing -> notAllowed
                  Just user2 
                     | user == user2 -> done
                     | True -> 
                         do
                            reuseVersion simpleDB user2 version
                            notAllowed
                                                  

-- | Put a version back into 'openVersions'
-- (used when a commit fails).
reuseVersion :: SimpleDB -> User -> ObjectVersion -> IO ()
reuseVersion simpleDB user version = 
   do
      userRef <- mkWeak user user 
         (Just (deleteVersion (openVersions simpleDB) version))
      atomicModifyIORef (openVersions simpleDB)
         (\ fm0 -> (addToFM fm0 version userRef,()))

-- | deleteVersion is only used internally.  It is possible (because
-- of the rules on finalizers) that it will be used when the
-- version is already deleted.
deleteVersion :: IORef (FiniteMap ObjectVersion a) -> ObjectVersion 
   -> IO ()
deleteVersion mapRef version =
   atomicModifyIORef mapRef (\ fm -> (delFromFM fm version,()))


flushVersion :: SimpleDB -> TXN -> IO ()
flushVersion simpleDB txn =
   do
      nextVersion <- readIORef (nextVersion simpleDB)
      setObjectHere1 (miscDB simpleDB) 0 txn nextVersion

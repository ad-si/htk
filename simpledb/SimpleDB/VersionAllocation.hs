-- | This module handles the allocation of 'ObjectVersion's.
-- In addition this manages the openVersions database.
module SimpleDB.VersionAllocation(
   initVersions,
   allocVersion,
   useVersion,
   reuseVersion,
   flushVersion,
   forgetUsersVersions,

   ) where

import Maybe

import Util.DeprecatedFiniteMap
import Data.IORef

import Data.IORef

import Util.Computation

import Server.PasswordFile(User)

import SimpleDB.ServerErrors
import SimpleDB.VersionInfo
import SimpleDB.Types
import SimpleDB.BDBOps
import SimpleDB.BDBExtras

initVersions :: BDB -> IO (IORef ObjectVersion)
initVersions bdb =
   do
      objectVersion <- getObject bdb 1
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
      userOpt <- atomicModifyIORef (openVersions simpleDB)
         (\ fm0 -> (delFromFM fm0 version,lookupFM fm0 version))
      let
         notAllowed = throwError AccessError
            "Attempt to commit version which is not allocated"

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
   atomicModifyIORef (openVersions simpleDB)
      (\ fm0 -> (addToFM fm0 version user,()))

-- | Forget all versions belonging to a user and free their memory.
forgetUsersVersions :: SimpleDB -> User -> IO ()
forgetUsersVersions simpleDB user0 =
   do
      l <- atomicModifyIORef (openVersions simpleDB)
         (\ fm0 ->
            let
               old :: [(ObjectVersion,User)]
               old = fmToList fm0

               toDelete :: [ObjectVersion]
               toDelete = mapMaybe
                  (\ (ov,user1) ->
                     if user1 == user0
                        then
                           Just ov
                        else
                           Nothing
                     )
                  old

               fm1 = foldl
                  (\ fm ov -> delFromFM fm ov)
                  fm0
                  toDelete
            in
               (fm1,sizeFM fm1)
            )

      seq l done

flushVersion :: SimpleDB -> TXN -> IO ()
flushVersion simpleDB txn =
   do
      nextVersion <- readIORef (nextVersion simpleDB)
      setObjectHere1 (miscDB simpleDB) 1 txn nextVersion

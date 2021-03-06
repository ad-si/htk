-- | Understands queries to the database,
module SimpleDB.QuerySimpleDB(
   querySimpleDB,
   ) where

import Control.Monad

import Data.IORef
import qualified Data.Map as Map

import Util.Computation

import Server.PasswordFile


import SimpleDB.Permissions
import SimpleDB.Types
import SimpleDB.ServerErrors
import SimpleDB.SecurityManagement
import SimpleDB.LocationAllocation
import SimpleDB.VersionAllocation
import SimpleDB.ModifyUserInfo
import SimpleDB.LastChange
import SimpleDB.GetDiffs
import SimpleDB.Retrieve
import SimpleDB.SetGetSecurityData
import SimpleDB.Commit


querySimpleDB :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB user simpleDB simpleDBCommand =
   catchError (querySimpleDB1 user simpleDB simpleDBCommand)
      (\ errorType mess -> IsError errorType mess)

querySimpleDB1 :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB1 user simpleDB command = case command of
   NewLocation ->
      do
         permissions <- getGlobalPermissions simpleDB
         verifyGlobalAccess user permissions WriteActivity
         location <- getNextLocation simpleDB user
         return (IsLocation location)
   NewVersion ->
      do
         permissions <- getGlobalPermissions simpleDB
         verifyGlobalAccess user permissions WriteActivity
         version <- allocVersion simpleDB user
         return (IsObjectVersion version)
   ListVersions ->
      do
         versionDataMap <- readIORef (versionData simpleDB)
         return (IsObjectVersions (Map.keys versionDataMap))
   Retrieve location version ->
      do
         icsl <- retrieve simpleDB user location version
         return (IsData icsl)
   LastChange version location ->
      do
         verifyAccess simpleDB user version (Just location)
            ReadActivity
         lastChangeVersion <- lastChange simpleDB version location
         return (IsObjectVersion lastChangeVersion)
   Commit versionInformation redirects changeData parentChanges ->
      do
         objectVersionOpt <- commit simpleDB user versionInformation redirects
            changeData parentChanges
         return (case objectVersionOpt of
            Nothing -> IsOK
            Just objectVersion -> IsObjectVersion objectVersion
            )
   ModifyUserInfo versionInformation ->
      do
         permissions <- getGlobalPermissions simpleDB
         verifyGlobalAccess user permissions WriteActivity
         case versionInformation of
            Version1 _ ->
               throwError MiscError "ModifyUserInfo may not specify Version1"
            Version1Plus _ _ -> throwError MiscError
               "ModifyUserInfo may not specify Version1Plus"
            _ -> modifyUserInfo simpleDB user versionInformation
   GetDiffs objectVersion parentVersions ->
      do
         (diffs1,diffs2) <- getDiffs simpleDB user objectVersion parentVersions
         return (IsDiffs diffs1 diffs2)
   GetPermissions ovLocOpt ->
      do
         permissions <- getPermissions simpleDB user ovLocOpt
         return (IsPermissions permissions)
   SetPermissions ovLocOpt permissions ->
      do
         setPermissions simpleDB user ovLocOpt permissions
         return (IsOK)
   GetParentLocation ov ->
      do
         parentLocationOpt <- getParentLocation simpleDB user ov
         case parentLocationOpt of
            Nothing -> return IsOK
            Just parentLocation -> return (IsLocation parentLocation)
   ClaimAdmin wantAdmin ->
      do
         if wantAdmin
            then
               do
                  success <- claimAdmin user
                  if success
                     then
                        done
                     else
                        throwError AccessError
                           "You are not entitled to ADMIN status"
            else
               revokeAdmin user
         return IsOK
   MultiCommand commands ->
      do
         -- NB.  We must use querySimpleDB instead of querySimpleDB1,
         -- so that errors can be caught and the remaining commands
         -- executed.
         responses <- mapM (querySimpleDB user simpleDB) commands
         return (MultiResponse responses)







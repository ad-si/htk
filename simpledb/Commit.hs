-- | This module implements the 'SimpleDBTypes.Commit' command.
module Commit(
   commit,
   ) where

import Control.Exception
import Data.FiniteMap

import Monad
import Maybe

import Computation(done)

import PasswordFile(User)

import BDBOps
import VersionInfo
import VersionState
import ServerErrors
import VersionInfo(ObjectVersion)
import SimpleDBTypes
import Permissions
import SecurityManagement
import PrimitiveLocation
import LocationAllocation
import ModifyUserInfo
import VersionData
import FlushSimpleDB

commit :: SimpleDB -> User -> VersionInformation 
   -> [(Location,Maybe ObjectVersion)] 
   -> [(Location,ChangeData)] 
   -> [(Location,Location)]
   -> IO (Maybe ObjectVersion)
commit simpleDB user versionInformation redirects0 changeData0 parentChanges =
   do
      permissions <- getGlobalPermissions simpleDB
      verifyGlobalAccess user permissions WriteActivity
      txn <- beginTransaction

      (versionOpt :: Maybe ObjectVersion) 
            <- Control.Exception.catch (

         -- The following action can end in 3 ways.
         -- (1) successfully, in which case we return Nothing.
         -- (2) unsuccessfully with an exception.  In this case we don't
         --     return at all.
         -- (3) unsuccessfully with an ObjectVersion, indicating a
         --     version with this ServerInfo is already known to us.  In
         --     this case we return (Left ObjectVersion).
         do
            let
               toParent :: UserInfo -> Maybe ObjectVersion
               toParent userInfo = case parents userInfo of
                  [] -> Nothing
                  parent:_ -> Just parent

               getVersionInfo :: ObjectVersion -> IO VersionInfo
               getVersionInfo objectVersion =
                  do
                     versionInfoOpt <- lookupVersionInfo 
                        (versionState simpleDB) objectVersion
                     case versionInfoOpt of
                        Nothing -> throwError MiscError 
                           ("Attempt to commit version with no "
                              ++ "known VersionInfo")
                        Just versionInfo -> return versionInfo

               wrap :: UserInfo -> IO (Maybe ObjectVersion,ObjectVersion)
               wrap userInfo = return (toParent userInfo,version userInfo) 

            (parentOpt1,thisVersion1) <- case versionInformation of
               UserInfo1 userInfo -> wrap userInfo
               VersionInfo1 versionInfo -> wrap (VersionInfo.user versionInfo)
               Version1 objectVersion ->
                  do
                     versionInfo <- getVersionInfo objectVersion
                     wrap (VersionInfo.user versionInfo)
               Version1Plus objectVersion parentVersion ->
                  do
                     -- Check if VersionInfo is known (even though
                     -- we aren't going to do anything with it).
                     getVersionInfo objectVersion
                     return (Just parentVersion,objectVersion)

            let
               -- This maps locations to the version from which they
               -- come from.
               redirectsMap :: FiniteMap Location (Maybe ObjectVersion)
               redirectsMap = listToFM redirects0

               locationSource :: Location -> Maybe ObjectVersion
               locationSource location = case lookupFM redirectsMap location of
                  Just versionOpt -> versionOpt
                  _ -> parentOpt1

               verifyLocation :: Location -> Activity -> IO ()
               verifyLocation location activity =
                  case locationSource location of
                     Nothing -> done
                     Just version -> verifyAccess simpleDB user
                        version (Just location) activity

            -- enter the new stuff in the BDB repository; also check
            -- permissions.
            (objectChanges1 :: [(Location,
                     Either BDBKey (ObjectVersion,Location))])
                  <- mapM
               (\ (location,newItem) ->
                  case newItem of
                     Left icsl ->
                        do
                           verifyLocation location WriteActivity
                           bdbKey <- writeBDB (dataDB simpleDB) txn icsl
                           return (location,Left bdbKey)
                     Right (objectLoc@(oldVersion,oldLocation)) ->
                        do
                           verifyAccess simpleDB user oldVersion 
                              (Just oldLocation) ReadActivity
                           return (location,Right objectLoc)
                  )
               changeData0

            (redirects' :: [(Location,Either ObjectVersion PrimitiveLocation)])
               <- mapM
                  (\ (location,redirect) ->
                     do
                        redirect' <- case redirect of
                           Just version -> return (Left version)
                           Nothing -> 
                              do
                                 location1 <- getNextLocation simpleDB
                                 return (Right (toPrimitiveLocation location1))
                        return (location,redirect')
                     )
                  redirects0

            -- Verify permissions for the parentChanges.
            mapM
               (\ (object,parent) ->
                  verifyLocation object PermissionsActivity
                  )         
               parentChanges

            versionOpt <- modifyUserInfo1 simpleDB user versionInformation txn
            if isJust versionOpt 
               then
                  do
                     let
                        frozenVersion = FrozenVersion {
                           parent' = parentOpt1,
                           thisVersion' = thisVersion1,
                           objectChanges = objectChanges1,
                           redirects' = redirects',
                           parentChanges = parentChanges
                           }
                     modifyVersionData simpleDB thisVersion1 
                        frozenVersion txn
               else
                  done
            return versionOpt

         )
         (\ exception ->
            do
               abortTransaction txn
               Control.Exception.throw exception
            )

      if isJust versionOpt
         then
            abortTransaction txn
         else
            flushSimpleDB simpleDB txn      
      return versionOpt

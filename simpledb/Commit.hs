-- | This module implements the 'SimpleDBTypes.Commit' command.
module Commit(
   commit,
   ) where

import Control.Exception

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
import LocationAllocation
import ModifyUserInfo
import VersionData
import FlushSimpleDB

commit :: SimpleDB -> User -> VersionInformation 
   -> [(Location,Either ObjectVersion (Maybe Location))] 
   -> [(Location,ChangeData)] 
   -> IO (Maybe ObjectVersion)
commit simpleDB user versionInformation redirects0 changeData0 =
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

            -- enter the new stuff in the BDB repository; also check
            -- permissions.
            (objectChanges1 :: [(Location,
                     Either BDBKey (ObjectVersion,Location))])
                  <- mapM
               (\ (location,newItem) ->
                  case newItem of
                     Left icsl ->
                        do
                           case parentOpt1 of
                              Just parentVersion -> 
                                 verifyAccess simpleDB user 
                                    parentVersion (Just location) WriteActivity
                              Nothing -> done
                           bdbKey <- writeBDB (dataDB simpleDB) txn icsl
                           return (location,Left bdbKey)
                     Right (objectLoc@(oldVersion,oldLocation)) ->
                        do
                           verifyAccess simpleDB user oldVersion 
                              (Just oldLocation) ReadActivity
                           return (location,Right objectLoc)
                  )
               changeData0

            let
               -- redirects which involve Left version
               redirectsA :: 
                  [(Location,Either ObjectVersion PrimitiveLocation)]
               redirectsA = mapMaybe
                  (\ (location,redirect) -> case redirect of
                     Left version -> Just (location,Left version)
                     Right _ -> Nothing
                     )
                  redirects0
            
               -- redirects which involve Right (PrimitiveLocation),
               -- to be fed to LocationAllocation.getRedirectLocations
               redirectsB1 :: [(Location,Maybe Location)]
               redirectsB1 = mapMaybe
                  (\ (location,redirect) -> case redirect of
                    Right parentLocationOpt 
                       -> Just (location,parentLocationOpt)
                    Left _ -> Nothing
                    )
                  redirects0

            (redirectsB2 :: [(Location,PrimitiveLocation)])
               <- getRedirectLocations simpleDB parentOpt1 redirectsB1

            let
               redirectsB 
                  :: [(Location,Either ObjectVersion PrimitiveLocation)]
               redirectsB =
                  map
                     (\ (location,primitiveLocation) ->
                        (location,Right primitiveLocation)
                        )
                     redirectsB2

            versionOpt <- modifyUserInfo1 simpleDB user versionInformation txn
            if isJust versionOpt 
               then
                  do
                     let
                        frozenVersion = FrozenVersion {
                           parent' = parentOpt1,
                           thisVersion' = thisVersion1,
                           objectChanges = objectChanges1,
                           redirects' = redirectsA ++ redirectsB
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

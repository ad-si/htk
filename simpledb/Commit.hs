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

            -- enter the new stuff in the BDB repository; also check
            -- permissions.
            (objectChanges1 :: [(Location,
                     Either BDBKey (ObjectVersion,Location))])
                  <- mapM
               (\ (location,newItem) ->
                  case newItem of
                     Left icsl ->
                        do
                           bdbKey <- writeBDB (dataDB simpleDB) txn icsl
                           return (location,Left bdbKey)
                     Right objectLoc -> return (location,Right objectLoc)
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
                                 location1 <- getNextLocation simpleDB user
                                 return (Right (toPrimitiveLocation location1))
                        return (location,redirect')
                     )
                  redirects0

            let
               frozenVersion = FrozenVersion {
                  parent' = parentOpt1,
                  thisVersion' = thisVersion1,
                  objectChanges = objectChanges1,
                  redirects' = redirects',
                  parentChanges = parentChanges
                  }
            (thisVersionData,commitVersionData) 
               <- modifyVersionData simpleDB thisVersion1 
                  frozenVersion txn
    

            -- do access checks.
            let
               verifyLocation :: Location -> Activity -> IO ()
               verifyLocation location activity = 
                  verifyMultiAccess simpleDB user thisVersion1 
                     (Just (thisVersionData,location)) [activity]

            mapM_
               (\ (location,item) ->
                  case item of
                     Left _ -> verifyLocation location WriteActivity
                     Right (oldVersion,oldLocation) -> 
                        verifyAccess simpleDB user oldVersion 
                           (Just oldLocation) ReadActivity
                  )
               objectChanges1

            mapM_
               (\ (location,redirect) ->
                  case redirect of
                     Left oldVersion -> verifyAccess simpleDB user oldVersion
                        (Just location) ReadActivity
                     Right _ -> done
                  )
               redirects'

            -- Verify permissions for the parentChanges.
            -- We specify that either an object be open (so it belongs to the
            -- user), or else the user must have Permissions access to it.
            mapM
               (\ (object,parent) ->
                  do
                     let
                        pLocation = retrievePrimitiveLocation1
                           (redirects thisVersionData) object

                     isOpen <- isOpenLocation simpleDB user pLocation
                     if isOpen
                        then
                           done
                        else
                           verifyLocation object PermissionsActivity
                  )
               parentChanges


            let
               usedLocations1 :: [PrimitiveLocation]
               usedLocations1 =
                  map
                     (\ (location,_) -> retrievePrimitiveLocation1
                        (redirects thisVersionData) location)
                     objectChanges1
                  
               usedLocations2 :: [PrimitiveLocation]
               usedLocations2 =
                  map
                     (\ (object,_) -> retrievePrimitiveLocation1
                        (redirects thisVersionData) object
                        )
                     parentChanges

            closeLocations simpleDB user (usedLocations1 ++ usedLocations2)

            versionOpt 
               <- modifyUserInfo1 simpleDB user versionInformation txn True
            if not (isJust versionOpt) 
               then
                  commitVersionData
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

-- | Understands queries to the database, 
module QuerySimpleDB(
   querySimpleDB,
   ) where

import Monad

import Data.IORef
import Data.FiniteMap

import Computation

import PasswordFile


import BDBOps
import Permissions
import SimpleDBTypes
import ServerErrors
import SecurityManagement
import VersionData
import PrimitiveLocation


querySimpleDB :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB user simpleDB simpleDBCommand =
   catchError (querySimpleDB user simpleDB simpleDBCommand)
      (\ errorType mess -> case errorType of
         AccessError -> IsAccess mess
         NotFoundError -> IsNotFound mess
         InternalError -> IsError ("Server Internal Error!!!\n" ++ mess)
         MiscError -> IsError mess
         ) 

querySimpleDB1 :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB1 user simpleDB command = case command of
   NewLocation parentOpt -> error "TBD"
   NewVersion -> error "TBD"
   ListVersions ->
      do
         versionDataMap <- readIORef (versionData simpleDB)
         return (IsObjectVersions (keysFM versionDataMap)) 
   Retrieve location version -> 
      do
         verifyAccess simpleDB user version (Just location)
            ReadActivity
         versionData <- getVersionData simpleDB version
         let
            primitiveLocation = retrievePrimitiveLocation versionData location
         key <- retrieveKey versionData primitiveLocation
         dataOpt <- readBDB (dataDB simpleDB) key
         case dataOpt of
            Nothing -> throwError InternalError "Unexpected missing data key"
            Just icsl -> return (IsData icsl)
   LastChange _ _ -> error "TBD"
   Commit _ _ _ -> error "TBD"
   ModifyUserInfo versionInfo -> error "TBD"
   GetDiffs _ _ -> error "TBD"
   GetPermissions Nothing -> 
      do
         permissions <- getGlobalPermissions simpleDB
         verifyGlobalGetPermissionsAccess user permissions
         return (IsPermissions permissions)
   GetPermissions (Just (version,location)) ->
      do
         verifyGetPermissionsAccess simpleDB user version (Just location)
         versionData <- getVersionData simpleDB version
         let
            primitiveLocation = retrievePrimitiveLocation versionData location
         securityData <- getSecurityData simpleDB primitiveLocation
         return (IsPermissions (permissions securityData))
   SetPermissions Nothing permissions ->
      do
         permissionsValidCheck permissions
         permissions0 <- getGlobalPermissions simpleDB
         verifyGlobalAccess user permissions0 PermissionsActivity
         setGlobalPermissions simpleDB permissions
         return IsOK
   SetPermissions (Just (version,location)) permissions ->
      do
         permissionsValidCheck permissions
         verifyAccess simpleDB user version (Just location) PermissionsActivity
         versionData <- getVersionData simpleDB version
         let
            primitiveLocation = retrievePrimitiveLocation versionData location
         securityData0 <- getSecurityData simpleDB primitiveLocation
         let
            securityData1 = securityData0 {permissions = permissions}
         setSecurityData simpleDB primitiveLocation securityData1
         return IsOK
   GetParentLocation (version,location) ->
      do
         verifyGetPermissionsAccess simpleDB user version (Just location)
         versionData <- getVersionData simpleDB version
         let
            pLocation = retrievePrimitiveLocation versionData location

         securityData <- getSecurityData simpleDB pLocation
         case parentOpt securityData of
            Nothing -> return IsOK
            Just pLocation2 ->
               return (IsLocation (retrieveLocation versionData pLocation2))
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

-- ----------------------------------------------------------------------------
-- Miscellaneous functions we need
-- ----------------------------------------------------------------------------

permissionsValidCheck :: Permissions -> IO ()
permissionsValidCheck permissions =
   if permissionsValid permissions
      then
         done
      else
         throwError MiscError "Permissions are invalid"

versionInformationToVersion :: VersionInformation -> ObjectVersion
versionInformationToVersion versionInformation = case versionInformation of
   UserInfo1 userInfo -> version userInfo
   VersionInfo1 versionInfo -> version (user versionInfo)
   Version1 objectVersion -> objectVersion



         

-- | This module implements the 'SimpleDBType.GetPermissions', 
-- 'SimpleDBType.SetPermissions' and 'SimpleDBType.GetParentLocation' 
-- operations
module SetGetSecurityData(
   getPermissions,
   setPermissions,
   getParentLocation,
   ) where

import PasswordFile(User)

import Permissions
import SecurityManagement
import VersionInfo
import PrimitiveLocation
import SimpleDBTypes
import VersionData


getPermissions :: SimpleDB -> User -> Maybe (ObjectVersion,Location) 
   -> IO Permissions
getPermissions simpleDB user ovLocOpt = case ovLocOpt of
   Nothing ->
      do
         permissions <- getGlobalPermissions simpleDB
         verifyGlobalGetPermissionsAccess user permissions
         return permissions
   Just (version,location) ->
      do
         verifyGetPermissionsAccess simpleDB user version (Just location)
         versionData <- getVersionData simpleDB version
         let
            primitiveLocation = retrievePrimitiveLocation versionData location
         securityData <- getSecurityData simpleDB primitiveLocation
         return (permissions securityData)

setPermissions :: SimpleDB -> User -> Maybe (ObjectVersion,Location) 
   -> Permissions -> IO ()
setPermissions simpleDB user ovLocOpt permissions =
   case ovLocOpt of
      Nothing ->
         do
         permissionsValidCheck permissions
         permissions0 <- getGlobalPermissions simpleDB
         verifyGlobalAccess user permissions0 PermissionsActivity
         setGlobalPermissions simpleDB permissions
      Just (version,location) ->
         do
            permissionsValidCheck permissions
            verifyAccess simpleDB user version (Just location) 
               PermissionsActivity
            versionData <- getVersionData simpleDB version
            let
               primitiveLocation 
                  = retrievePrimitiveLocation versionData location
            securityData0 <- getSecurityData simpleDB primitiveLocation
            let
               securityData1 = securityData0 {permissions = permissions}
            setSecurityData simpleDB primitiveLocation securityData1


-- *** NB.  Do not allow the user to SET a parent location without checking
-- that this does not introduce cycles.
getParentLocation :: SimpleDB -> User -> (ObjectVersion,Location) 
   -> IO (Maybe Location)
getParentLocation simpleDB user (version,location) =
   do
      verifyGetPermissionsAccess simpleDB user version (Just location)
      versionData <- getVersionData simpleDB version
      let
         pLocation = retrievePrimitiveLocation versionData location

      securityData <- getSecurityData simpleDB pLocation
      return (fmap (retrieveLocation versionData) (parentOpt securityData))
 
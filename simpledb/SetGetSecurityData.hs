-- | This module implements the 'SimpleDBType.GetPermissions', 
-- 'SimpleDBType.SetPermissions' and 'SimpleDBType.GetParentLocation' 
-- operations
module SetGetSecurityData(
   getPermissions,
   setPermissions,
   getParentLocation,
   ) where

import Data.FiniteMap

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
         getPermissions1 simpleDB primitiveLocation

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
            setPermissions1 simpleDB primitiveLocation permissions


-- *** NB.  Do not allow the user to SET a parent location without checking
-- that this does not introduce cycles.
getParentLocation :: SimpleDB -> User -> (ObjectVersion,Location) 
   -> IO (Maybe Location)
getParentLocation simpleDB user (version,location) =
   do
      verifyGetPermissionsAccess simpleDB user version (Just location)
      versionData <- getVersionData simpleDB version
      return (lookupFM (parentsMap versionData) location)
 
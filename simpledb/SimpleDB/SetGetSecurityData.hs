-- | This module implements the 'SimpleDBType.GetPermissions',
-- 'SimpleDBType.SetPermissions' and 'SimpleDBType.GetParentLocation'
-- operations
module SimpleDB.SetGetSecurityData(
   getPermissions,
   setPermissions,
   getParentLocation,
   ) where

import Data.Maybe

import Util.DeprecatedFiniteMap

import Server.PasswordFile(User)

import SimpleDB.Permissions
import SimpleDB.SecurityManagement
import SimpleDB.VersionInfo
import SimpleDB.PrimitiveLocation
import SimpleDB.Types
import SimpleDB.VersionData


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
--
-- We only permit this if the user has get-permissions access (that is,
-- either read or permissions access) to either this object's parent, or
-- (if no parent exists) the object itself.  NB it is allowed it
-- the object is unreadable but the parent is; this is useful for the
-- uni-types package since it uses this function to get at the parent when
-- creating a replacement NoAccessObject.
getParentLocation :: SimpleDB -> User -> (ObjectVersion,Location)
   -> IO (Maybe Location)
getParentLocation simpleDB user (version,location) =
   do
      versionData <- getVersionData simpleDB version
      let
         parentLocationOpt = lookupFM (parentsMap versionData) location

         locationToTest = fromMaybe location parentLocationOpt

      verifyGetPermissionsAccess simpleDB user version (Just locationToTest)

      return parentLocationOpt



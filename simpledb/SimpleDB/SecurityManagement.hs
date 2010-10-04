-- | The functions in this module manage and verify access to the
-- database.
module SimpleDB.SecurityManagement(
   setPermissions1,
   getPermissions1,
   setGlobalPermissions,
   getGlobalPermissions,
   verifyAccess,
   verifyMultiAccess,
   verifyGetPermissionsAccess,
   verifyGlobalAccess,
   verifyGlobalGetPermissionsAccess,
   ) where

import Data.Maybe

import qualified Data.Map as Map

import Util.Computation(done)

import Server.PasswordFile
import Server.GroupFile

import SimpleDB.ServerErrors
import SimpleDB.Permissions
import SimpleDB.ExaminePermissions
import SimpleDB.Types
import SimpleDB.BDBOps
import SimpleDB.BDBExtras
import SimpleDB.PrimitiveLocation
import SimpleDB.VersionInfo(ObjectVersion)
import SimpleDB.VersionData

-- ----------------------------------------------------------------------
-- Accessing and retrieving permissions
-- ----------------------------------------------------------------------

setPermissions1 :: SimpleDB -> PrimitiveLocation -> Permissions -> IO ()
setPermissions1 simpleDB (PrimitiveLocation key) permissions =
   do
      setObjectHere (securityDB simpleDB) (fromIntegral key) permissions
      flushBDB (securityDB simpleDB)

getPermissions1 :: SimpleDB -> PrimitiveLocation -> IO Permissions
getPermissions1 simpleDB (PrimitiveLocation key) =
   getObject (securityDB simpleDB) (fromIntegral key)

getGlobalPermissions :: SimpleDB -> IO Permissions
getGlobalPermissions simpleDB = getObject (miscDB simpleDB) 3

setGlobalPermissions :: SimpleDB -> Permissions -> IO ()
setGlobalPermissions simpleDB permissions =
   do
      setObjectHere (miscDB simpleDB) 3 permissions
      flushBDB (miscDB simpleDB)

-- ----------------------------------------------------------------------
-- Verifying Access
-- ----------------------------------------------------------------------


verifyAccess :: SimpleDB -> User -> ObjectVersion -> Maybe Location
   -> Activity -> IO ()
verifyAccess simpleDB user objectVersion locationOpt activity =
   do
      vLocationOpt <- getVLocation simpleDB objectVersion locationOpt
      verifyMultiAccess simpleDB user objectVersion vLocationOpt [activity]

verifyGetPermissionsAccess :: SimpleDB -> User -> ObjectVersion
   -> Maybe Location -> IO ()
verifyGetPermissionsAccess simpleDB user objectVersion locationOpt =
   do
      vLocationOpt <- getVLocation simpleDB objectVersion locationOpt
      verifyMultiAccess simpleDB user objectVersion vLocationOpt
         [ReadActivity,PermissionsActivity]

getVLocation :: SimpleDB -> ObjectVersion -> Maybe Location
   -> IO (Maybe (VersionData,Location))
getVLocation simpleDB version Nothing = return Nothing
getVLocation simpleDB version (Just location ) =
   do
      versionData <- getVersionData simpleDB version
      return (Just (versionData,location))

-- | Verify if /any/ of the given activities are permissable.
verifyMultiAccess :: SimpleDB -> User -> ObjectVersion
   -> Maybe (VersionData,Location) -> [Activity] -> IO ()
verifyMultiAccess simpleDB user version vLocationOpt activities =
   do
      groupFile <- getGroupFile
      verifyMultiAccess1 simpleDB groupFile user
         version vLocationOpt activities

verifyMultiAccess1 :: SimpleDB -> GroupFile -> User -> ObjectVersion
   -> Maybe (VersionData,Location) -> [Activity] -> IO ()
verifyMultiAccess1 simpleDB groupFile user objectVersion vLocationOpt
      activities =
   case activities of
      [] ->
         do
            isAdmin <- getAdminStatus user
            if isAdmin
               then
                  done
               else
                  throwError AccessError (
                     "Access to object in version "
                     ++ show objectVersion
                     ++ " denied")
      (activity : activities1) ->
         do
            access0 <-
               verifyAccess0 simpleDB groupFile (userId user)
                  objectVersion vLocationOpt activity
            if access0
               then
                  done
               else
                  verifyMultiAccess1 simpleDB groupFile user objectVersion
                     vLocationOpt activities1

verifyAccess0 :: SimpleDB -> GroupFile -> String -> ObjectVersion
   -> Maybe (VersionData,Location) -> Activity -> IO Bool
verifyAccess0 simpleDB groupFile userId0 objectVersion vLocationOpt activity =
   do
      allowedOpt <- case vLocationOpt of
         Nothing -> return Nothing
         Just (versionData,location) ->
            verifyAccess1 simpleDB groupFile userId0 objectVersion
               versionData location activity
      case allowedOpt of
         Just allowed -> return allowed
         Nothing ->
            do
               permissions0 <- getGlobalPermissions simpleDB
               result1 <- examinePermissions (versionState simpleDB)
                  groupFile userId0 objectVersion activity permissions0
               return (fromMaybe True result1)

verifyAccess1 :: SimpleDB -> GroupFile -> String -> ObjectVersion
   -> VersionData -> Location -> Activity -> IO (Maybe Bool)
verifyAccess1 simpleDB groupFile userId0 version versionData location1
      activity =
   do
      let
         plocation1 = retrievePrimitiveLocation versionData location1

      permissions <- getPermissions1 simpleDB plocation1
      allowedOpt <- examinePermissions (versionState simpleDB)
         groupFile userId0 version activity permissions
      case allowedOpt of
         Just _ -> return allowedOpt
         Nothing ->
            case Map.lookup location1 (parentsMap versionData) of
               Nothing -> return Nothing
               Just location2 ->
                  verifyAccess1 simpleDB groupFile userId0 version versionData
                     location2 activity

verifyGlobalAccess :: User -> Permissions -> Activity -> IO ()
verifyGlobalAccess user permissions activity =
   verifyGlobalMultiAccess user permissions [activity]

verifyGlobalGetPermissionsAccess :: User -> Permissions -> IO ()
verifyGlobalGetPermissionsAccess user permissions =
   verifyGlobalMultiAccess user permissions [ReadActivity,PermissionsActivity]

verifyGlobalMultiAccess :: User -> Permissions -> [Activity] -> IO ()
verifyGlobalMultiAccess user permissions activities =
   do
      groupFile <- getGroupFile
      verifyGlobalMultiAccess1 groupFile user permissions activities

verifyGlobalMultiAccess1 :: GroupFile -> User -> Permissions -> [Activity]
   -> IO ()
verifyGlobalMultiAccess1 groupFile user permissions activities =
   case activities of
      [] ->
         do
            isAdmin <- getAdminStatus user
            if isAdmin
               then
                  done
               else
                  throwError AccessError "Global access to repository denied"
      (activity:activities1) ->
         do
            let
               grantedOpt = examineGlobalPermissions groupFile
                  (userId user) activity permissions
               granted = fromMaybe True grantedOpt
            if granted
               then
                  done
               else
                  verifyGlobalMultiAccess1
                     groupFile user permissions activities1

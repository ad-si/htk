-- | The functions in this module manage and verify access to the 
-- database.
module SecurityManagement(
   setPermissions1,
   getPermissions1,
   setGlobalPermissions,
   getGlobalPermissions,
   verifyAccess,   
   verifyGetPermissionsAccess,
   verifyGlobalAccess,
   verifyGlobalGetPermissionsAccess,
   ) where

import Maybe

import Data.FiniteMap

import Computation(done)

import PasswordFile
import GroupFile

import ServerErrors
import Permissions
import ExaminePermissions
import SimpleDBTypes
import BDBOps
import BDBExtras
import PrimitiveLocation
import VersionInfo(ObjectVersion)
import VersionData

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
   verifyMultiAccess simpleDB user objectVersion locationOpt [activity]

verifyGetPermissionsAccess :: SimpleDB -> User -> ObjectVersion 
   -> Maybe Location -> IO ()
verifyGetPermissionsAccess simpleDB user objectVersion locationOpt =
   verifyMultiAccess simpleDB user objectVersion locationOpt
      [ReadActivity,PermissionsActivity]

verifyMultiAccess :: SimpleDB -> User -> ObjectVersion -> Maybe Location 
   -> [Activity] -> IO ()
verifyMultiAccess simpleDB user version locationOpt activities =
   do
      groupFile <- getGroupFile
      verifyMultiAccess1 simpleDB groupFile user 
         version locationOpt activities

verifyMultiAccess1 :: SimpleDB -> GroupFile -> User -> ObjectVersion 
   -> Maybe Location -> [Activity] -> IO ()
verifyMultiAccess1 simpleDB groupFile user objectVersion locationOpt 
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
               verifyAccess0 simpleDB groupFile (PasswordFile.userId user) 
                  objectVersion locationOpt activity
            if access0
               then
                  done
               else
                  verifyMultiAccess1 simpleDB groupFile user objectVersion 
                     locationOpt activities1

verifyAccess0 :: SimpleDB -> GroupFile -> String -> ObjectVersion 
   -> Maybe Location -> Activity -> IO Bool
verifyAccess0 simpleDB groupFile userId0 objectVersion locationOpt activity =
   do
      allowedOpt <- case locationOpt of
         Nothing -> return Nothing
         Just location ->
            do
               versionData <- getVersionData simpleDB objectVersion
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
            case lookupFM (parentsMap versionData) location1 of
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
                  (PasswordFile.userId user) activity permissions
               granted = fromMaybe True grantedOpt
            if granted
               then
                  done
               else
                  verifyGlobalMultiAccess1 
                     groupFile user permissions activities1
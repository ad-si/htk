-- | The functions in this module manage and verify access to the 
-- database.
module SecurityManagement(
   setSecurityData,
   getSecurityData,
   setGlobalPermissions,
   getGlobalPermissions,
   trivialSecurityData,
   verifyAccess,   
   verifyGetPermissionsAccess,
   verifyGlobalAccess,
   verifyGlobalGetPermissionsAccess,
   ) where

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

setSecurityData :: SimpleDB -> PrimitiveLocation -> SecurityData -> IO ()
setSecurityData simpleDB (PrimitiveLocation key) securityData =
   do
      setObjectHere (securityDB simpleDB) (fromIntegral key) securityData
      flushBDB (securityDB simpleDB)

getSecurityData :: SimpleDB -> PrimitiveLocation -> IO SecurityData
getSecurityData simpleDB (PrimitiveLocation key) =
   getObject (securityDB simpleDB) (fromIntegral key) 

getGlobalPermissions :: SimpleDB -> IO Permissions
getGlobalPermissions simpleDB = getObject (miscDB simpleDB) 2

setGlobalPermissions :: SimpleDB -> Permissions -> IO ()
setGlobalPermissions simpleDB permissions =
   do
      setObjectHere (miscDB simpleDB) 2 permissions 
      flushBDB (miscDB simpleDB)


trivialSecurityData :: Maybe PrimitiveLocation -> SecurityData
trivialSecurityData parentPrimitiveLocOpt =
   SecurityData {
      parentOpt = parentPrimitiveLocOpt,
      permissions = []
      }

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
      let
         examinePermissions0 = examinePermissions (versionState simpleDB)
            groupFile userId0 objectVersion activity 

      permissions0 <- getGlobalPermissions simpleDB
      result1 <- examinePermissions0 permissions0
      if result1
         then
            case locationOpt of
               Nothing -> return True
               Just location ->
                  do
                     versionData <- getVersionData simpleDB objectVersion
                     let
                        plocation = retrievePrimitiveLocation versionData
                           location
                     verifyAccess1 simpleDB groupFile userId0 objectVersion 
                        versionData plocation activity
         else
            return False 

verifyAccess1 :: SimpleDB -> GroupFile -> String -> ObjectVersion 
   -> VersionData -> PrimitiveLocation -> Activity -> IO Bool
verifyAccess1 simpleDB groupFile userId0 version versionData plocation1 
      activity =
   do
      securityData <- getSecurityData simpleDB plocation1
      allowed <- examinePermissions (versionState simpleDB)
         groupFile userId0 version activity (permissions securityData)
      if allowed
         then
            case parentOpt securityData of
               Nothing -> return True
               Just plocation2 ->
                  verifyAccess1 simpleDB groupFile userId0 version versionData
                     plocation2 activity
         else
            return False

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
               granted = examineGlobalPermissions groupFile
                  (PasswordFile.userId user) activity permissions
            if granted
               then
                  done
               else
                  verifyGlobalMultiAccess1 
                     groupFile user permissions activities1
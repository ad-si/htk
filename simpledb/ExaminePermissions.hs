-- | 'ExaminePermissions.examinePermissions' examines a 
-- 'Permissions.Permissions' and determines whether they either grant or
-- deny access.
module ExaminePermissions(
   examinePermissions,
   examineGlobalPermissions,
   ) where

import GroupFile
import PasswordFile

import Permissions
import VersionInfo

examinePermissions :: VersionState -> GroupFile -> String
   -> ObjectVersion -> Activity -> Permissions -> IO Bool
examinePermissions versionState groupFile userId version activity 
      permissions0 =
   case permissions0 of
      [] -> return True
      (permission : permissions1) ->
         do
            resultOpt <- examinePermission versionState groupFile userId 
               version activity permission
            case resultOpt of
               Nothing -> examinePermissions versionState groupFile 
                  userId version activity permissions1
               Just result -> return result

examinePermission :: VersionState -> GroupFile -> String 
   -> ObjectVersion -> Activity -> Permission -> IO (Maybe Bool)
examinePermission versionState groupFile userId version activity 
      permission =
   let
      activityIsIn = elem activity (activities permission)

      userIsIn = case domain permission of
         Nothing -> True
         Just (GroupFile.User userId0) -> userId0 == userId
         Just (Group group) -> userIsInGroup groupFile userId group
   in
      if activityIsIn && userIsIn
         then
            do
               let
                  isAncestor = versionIsAncestor versionState

               -- most expensive and therefore last test
               versionIsIn <- case 
                     (fromVersion permission,toVersion permission) of
                  (Nothing,Nothing) -> return True
                  (Just fromVersion0,Nothing) 
                     -> isAncestor fromVersion0 version
                  (Nothing,Just toVersion0)
                     -> isAncestor version toVersion0
                  (Just fromVersion0,Just toVersion0) 
                     -> if fromVersion0 == toVersion0
                        then
                           return (fromVersion0 == version)
                        else
                           do
                              fromAncestor <- isAncestor fromVersion0 version
                              if fromAncestor
                                 then
                                    isAncestor version toVersion0
                                 else
                                    return False
               return (
                  if versionIsIn then Just (grant permission) else Nothing)
                   
         else
            return Nothing

examineGlobalPermissions :: GroupFile -> String -> Activity -> Permissions 
   -> Bool
examineGlobalPermissions _ _ _ [] = True
examineGlobalPermissions groupFile userId activity (permission:permissions) =
   let
      activityIsIn = elem activity (activities permission)

      userIsIn = case domain permission of
         Nothing -> True
         Just (GroupFile.User userId0) -> userId0 == userId
         Just (Group group) -> userIsInGroup groupFile userId group
   in
      if activityIsIn && userIsIn
         then
            grant permission
         else
            examineGlobalPermissions groupFile userId activity permissions

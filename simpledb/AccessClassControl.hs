-- | This is the module that implements checking whether an operation is
-- permitted a user for a particular access class.
module AccessClassControl(
   verifyPermissions,
   verifyAccessClass,
   ) where

import GroupFile

import AccessClass

import {-# SOURCE #-} SimpleDBServer


verifyAccessClass ::
   GroupFile -- ^ the current group file
   -> String -- ^ the user whose access is to be controlled.
   -> Activity -- ^ the activity the user is attempting
   -> (Location -> IO AccessClass) 
      -- ^ an action for looking up parents of AccessClasses.
   -> AccessClass
   -> IO Bool
      -- ^ `True` if access is to be granted.
verifyAccessClass groupFile user activity getAccessClass accessClass =
   do
      let
         permissionOpt = verifyPermissions groupFile user activity 
            (permissions accessClass)
      case (permissionOpt,parent accessClass) of
         (Just permission,_) -> return permission
         (Nothing,Just parentLocation) ->
            do
               parentAccessClass <- getAccessClass parentLocation
               verifyAccessClass groupFile user activity getAccessClass 
                  parentAccessClass
         (Nothing,Nothing) -> return True

verifyPermissions :: 
   GroupFile -- ^ the current group file
   -> String -- ^ the user
   -> Activity -- ^ the activity the user is attempting.
   -> Permissions 
   -> Maybe Bool
verifyPermissions groupFile user activity [] = Nothing
verifyPermissions groupFile user activity (permission:permissions) =
   let
      activityApplies = elem activity (activities permission)

      userApplies = case domain permission of
         Nothing -> True
         Just (User user1) -> user == user1
         Just (Group group) -> userIsInGroup groupFile user group
   in
      if activityApplies && userApplies
         then
            Just (grant permission)
         else
            verifyPermissions groupFile user activity permissions  
   
-- | This module implements the security operations for the MMiSS API server
module MMiSSSecurityOps(
   getPermissions,
   setPermissions,
   setAdminStatus,
   ) where

import VariableSet(HasKey(..))

import Computation

import qualified VersionDB
import VersionInfo
import Permissions

import VersionGraph
import LinkManager
import ViewType
import ObjectTypes(WrappedLink)

import MMiSSRequest
import MMiSSSessionState
import MMiSSImportExportErrors
import MMiSSGetPut

getPermissions :: MMiSSSessionState -> GetPermissions 
   -> IO GetPermissionsResponse
getPermissions state (GetPermissions whichPermissions) =
   do
      (repository,ovOpt) <- decodeWhichPermissions state whichPermissions
      permissions <- VersionDB.getPermissions repository ovOpt
      return (GetPermissionsResponse (Permissions (
         unparsePermissions permissions)))
            
      
setPermissions :: MMiSSSessionState -> SetPermissions 
   -> IO SetPermissionsResponse
setPermissions state (SetPermissions whichPermissions (Permissions str)) =
   do
      let
         permissionsWE = parsePermissions str
      permissions <- coerceWithErrorOrBreakIO importExportError permissionsWE
      (repository,ovOpt) <- decodeWhichPermissions state whichPermissions
      VersionDB.setPermissions repository ovOpt permissions
      return SetPermissionsResponse

setAdminStatus :: MMiSSSessionState -> SetAdminStatus
   -> IO SetAdminStatusResponse
setAdminStatus state (SetAdminStatus
      (SetAdminStatus_Attrs setAdminStatusAction) serverRef) =
   do
      versionGraph <- lookupVersionGraph state serverRef
      let
         repository :: VersionDB.Repository
         repository = toVersionGraphRepository versionGraph

         isClaim = case setAdminStatusAction of
            SetAdminStatus_action_claim -> True
            SetAdminStatus_action_revoke -> False

      VersionDB.setAdminStatus repository isClaim
      return SetAdminStatusResponse

decodeWhichPermissions 
   :: MMiSSSessionState -> WhichPermissions 
   -> IO (VersionDB.Repository,Maybe (ObjectVersion,VersionDB.Location))
decodeWhichPermissions state whichPermissions =
   case whichPermissions of
      WhichPermissionsServerRef serverRef ->
         do
            versionGraph <- lookupVersionGraph state serverRef
            return (toVersionGraphRepository versionGraph,Nothing)
      WhichPermissionsVersionRef_ObjectFullName (versionRef,objectFullName) ->
         do
            view <- lookupView state versionRef  
            linkedObject <- getLinkedObject view objectFullName
            let
               wrappedLink :: WrappedLink
               wrappedLink = toWrappedLink linkedObject
               
               location :: VersionDB.Location
               location = toKey wrappedLink

            (Just objectVersion) <- getParentVersion view 
            return (repository view,Just (objectVersion,location))
-- | This module contains the functions for checking out a version, committing
-- it, and doing various other things. 
module MMiSSCheckOutCommit (
   checkOut,
   changeUserInfo,
   commitVersion,
   closeVersion,
   ) where

import AtomString
import Computation
import ExtendedPrelude
import Broadcaster
import Sources

import qualified VersionInfo

import VersionDB
import VersionGraph
import View hiding (setUserInfo) 
   -- ours here also allows default labels and so on 
import ViewType

import MMiSSImportExportErrors

import MMiSSRequest
import MMiSSSessionState
import MMiSSMapVersionInfo


-- ----------------------------------------------------------------------------
-- Checking out
-- --------------------------------------------------------------------------

checkOut :: MMiSSSessionState -> CheckOut -> IO CheckOutResponse
checkOut state (CheckOut (CheckOut_Attrs {checkOutVersion = versionStr} )
      serverRef versionRefOpt) =
   do
      let
         objectVersionWE = fromStringWE versionStr
      objectVersion <- case fromWithError objectVersionWE of
         Left _ -> importExportError "Version must be a number"
         Right objectVersion -> return objectVersion

      versionGraph <- lookupVersionGraph state serverRef
      let
         repository = toVersionGraphRepository versionGraph
         versionSimpleGraph = toVersionGraphGraph versionGraph

      viewOpt <- catchNotFound (
         getView repository versionSimpleGraph objectVersion)
      versionRef <- case viewOpt of
         Nothing -> importExportError "Version not found"
         Just view -> setView state versionRefOpt view
      return (CheckOutResponse versionRef)

-- ----------------------------------------------------------------------------
-- Changing the User Info
-- --------------------------------------------------------------------------

changeUserInfo :: MMiSSSessionState -> ChangeUserInfo 
   -> IO ChangeUserInfoResponse
changeUserInfo state (ChangeUserInfo versionRef userInfo) =
   do
      view <- lookupView state versionRef
      setUserInfo view userInfo
      return ChangeUserInfoResponse

-- ----------------------------------------------------------------------------
-- Committing
-- --------------------------------------------------------------------------

commitVersion :: MMiSSSessionState -> CommitVersion -> IO CommitVersionResponse
commitVersion state (CommitVersion versionRef userInfoOpt) =
   do
      view <- lookupView state versionRef
      case userInfoOpt of
         Nothing -> done
         Just userInfo -> setUserInfo view userInfo
      commitView view
      return CommitVersionResponse

-- ----------------------------------------------------------------------------
-- Closing a version
-- --------------------------------------------------------------------------

closeVersion :: MMiSSSessionState -> CloseVersion -> IO CloseVersionResponse
closeVersion state (CloseVersion versionRef) =
   do
      deleteView state versionRef
      return CloseVersionResponse

-- ----------------------------------------------------------------------------
-- Setting UserInfo
-- ----------------------------------------------------------------------------

setUserInfo :: View -> UserInfo -> IO ()
setUserInfo view userInfo1 =
   -- NB.  We won't get a concurrency problem here because two threads
   -- don't have access to the same view.
   do
      checkUserInfo userInfo1
      let
         broadcaster = viewInfoBroadcaster view
      viewInfo0 <- readContents broadcaster
      let
         user0 = VersionInfo.user viewInfo0
         user1 = toOurUserInfo user0 userInfo1

      user1 `seq` done 
         -- make sure any parsing errors get picked up in time. 

      broadcast broadcaster (viewInfo0 {VersionInfo.user = user1})

checkUserInfo :: UserInfo -> IO ()
checkUserInfo userInfo =
   case (userInfoVersion userInfo,userInfoParents userInfo) of
      (Nothing,Nothing) -> done
      _ -> importExportError 
         "You aren't allowed to set the version number or parents of a version"
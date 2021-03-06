-- | This module contains the functions for checking out a version, committing
-- it, and doing various other things.
module MMiSS.API.CheckOutCommit (
   checkOut,
   changeUserInfo,
   commitVersion,
   closeVersion,
   ) where

import Util.AtomString
import Util.Computation
import Util.Broadcaster
import Util.Sources

import qualified SimpleDB.VersionInfo as VersionInfo

import Types.VersionDB
import Types.VersionGraph
import Types.View hiding (setUserInfo)
   -- ours here also allows default labels and so on
import Types.ViewType

import MMiSS.ImportExportErrors

import MMiSS.API.Request
import MMiSS.API.SessionState
import MMiSS.API.MapVersionInfo


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
         versionGraphClient = toVersionGraphClient versionGraph

      viewOpt <- catchNotFound (
         getView repository versionGraphClient objectVersion)
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
checkUserInfo (UserInfo userInfo_attrs _) =
   case (userInfoVersion userInfo_attrs,userInfoParents userInfo_attrs) of
      (Nothing,Nothing) -> done
      _ -> importExportError
         "You aren't allowed to set the version number or parents of a version"

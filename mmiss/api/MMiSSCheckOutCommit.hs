{- This module contains the functions for checking out a version, committing
   it, and doing various other things. -}
module MMiSSCheckOutCommit (
   checkOut,
   changeUserInfo,
   commitVersion,
   closeVersion,
   ) where

import MMiSSRequest
import MMiSSSessionState

-- ----------------------------------------------------------------------------
-- Checking out
-- --------------------------------------------------------------------------

checkOut :: MMiSSSessionState -> CheckOut -> IO CheckOutResponse
checkOut = error "TBD"

-- ----------------------------------------------------------------------------
-- Changing the User Info
-- --------------------------------------------------------------------------

changeUserInfo :: MMiSSSessionState -> ChangeUserInfo 
   -> IO ChangeUserInfoResponse
changeUserInfo = error "TBD"

-- ----------------------------------------------------------------------------
-- Committing
-- --------------------------------------------------------------------------

commitVersion :: MMiSSSessionState -> CommitVersion -> IO CommitVersionResponse
commitVersion = error "TBD"

-- ----------------------------------------------------------------------------
-- Closing a version
-- --------------------------------------------------------------------------

closeVersion :: MMiSSSessionState -> CloseVersion -> IO CloseVersionResponse
closeVersion state (CloseVersion versionRef) =
   do
      deleteView state versionRef
      return CloseVersionResponse


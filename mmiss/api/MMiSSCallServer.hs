{- This module contains the functions for connecting to a server, closing 
   it, and listing versions. -}
module MMiSSCallServer(
   connect,
   closeServer,
   listVersions,
   ) where

import Messages

import qualified PasswordFile

import MMiSSRequest
import MMiSSSessionState

-- ----------------------------------------------------------------------------
-- Connecting
-- --------------------------------------------------------------------------

connect :: MMiSSSessionState -> Connect -> PasswordFile.User 
   -> IO ConnectResponse
connect state (Connect attrs serverRefOpt) user = 
   case (connectServer attrs,connectPassword attrs) of
      (Just serverStr,Just password) ->
         do
            let
               userId = case connectUser attrs of
                  Nothing -> PasswordFile.userId user
                  Just userId -> userId

      
    
            error "TBD"
      _ ->
         connectFailed "Server and password must both be specified!"
            -- this may change if allow no server (for the internal server)
            -- or no password (if inherited from this session or internal)

connectFailed :: String -> IO ConnectResponse
connectFailed mess =
   do 
      errorMess mess
      return (ConnectResponse Nothing)

-- --------------------------------------------------------------------------
-- Closing
-- --------------------------------------------------------------------------

closeServer :: MMiSSSessionState -> CloseServer -> IO CloseServerResponse
closeServer = error "TBD"

-- --------------------------------------------------------------------------
-- Listing versions
-- --------------------------------------------------------------------------

listVersions :: MMiSSSessionState -> ListVersions -> IO ListVersionsResponse
listVersions = error "TBD"
{- This module contains the functions for connecting to a server, closing 
   it, and listing versions. -}
module MMiSSCallServer(
   connect,
   closeServer,
   listVersions,
   ) where

import MMiSSRequest
import MMiSSSessionState

-- ----------------------------------------------------------------------------
-- Connecting
-- --------------------------------------------------------------------------

connect :: MMiSSSessionState -> Connect -> IO ConnectResponse
connect = error "TBD"

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
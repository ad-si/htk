module MMiSSGetPut(
   getObject,
   putObject,
   ) where

import MMiSSRequest
import MMiSSSessionState
import MMiSSAPIBlock

-- ----------------------------------------------------------------------------
-- Getting files
-- --------------------------------------------------------------------------

getObject :: MMiSSSessionState -> GetObject -> Block 
   -> IO (GetObjectResponse,Block)
getObject = error "TBD"

-- ----------------------------------------------------------------------------
-- Putting files
-- --------------------------------------------------------------------------

putObject :: MMiSSSessionState -> PutObject -> Block -> IO PutObjectResponse
putObject = error "TBD"


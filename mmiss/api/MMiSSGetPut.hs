module MMiSSGetPut(
   getObject,
   putObject,
   ) where

import MMiSSRequest
import MMiSSSessionState

-- ----------------------------------------------------------------------------
-- Getting files
-- --------------------------------------------------------------------------

getObject :: MMiSSSessionState -> GetObject -> IO GetObjectResponse
getObject = error "TBD"

-- ----------------------------------------------------------------------------
-- Putting files
-- --------------------------------------------------------------------------

putObject :: MMiSSSessionState -> PutObject -> IO PutObjectResponse
putObject = error "TBD"


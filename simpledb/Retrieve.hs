-- | 'Retrieve.retrieve' implements the Retrieve command, but is also
-- used by GetDiffs.
module Retrieve(
   retrieve,
   ) where

import ICStringLen

import PasswordFile(User)

import BDBOps
import VersionInfo
import Permissions
import SimpleDBTypes
import SecurityManagement
import ServerErrors
import VersionData
import PrimitiveLocation

retrieve :: SimpleDB -> User -> ObjectVersion -> Location -> IO ICStringLen
retrieve simpleDB user version location =
   do
      verifyAccess simpleDB user version (Just location)
         ReadActivity
      versionData <- getVersionData simpleDB version
      let
         primitiveLocation = retrievePrimitiveLocation versionData location
      key <- retrieveKey versionData primitiveLocation
      dataOpt <- readBDB (dataDB simpleDB) key
      case dataOpt of
         Nothing -> throwError InternalError "Unexpected missing data key"
         Just icsl -> return icsl

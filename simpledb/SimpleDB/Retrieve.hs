-- | 'Retrieve.retrieve' implements the Retrieve command, but is also
-- used by GetDiffs.
module SimpleDB.Retrieve(
   retrieve,
   ) where

import Util.ICStringLen

import Server.PasswordFile(User)

import SimpleDB.BDBOps
import SimpleDB.VersionInfo
import SimpleDB.Permissions
import SimpleDB.Types
import SimpleDB.SecurityManagement
import SimpleDB.ServerErrors
import SimpleDB.VersionData
import SimpleDB.PrimitiveLocation

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

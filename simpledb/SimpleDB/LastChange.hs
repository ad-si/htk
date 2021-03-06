-- | This module implements the LastChange command.
module SimpleDB.LastChange(
   lastChange,
   ) where

import SimpleDB.Types
import SimpleDB.VersionInfo
import SimpleDB.PrimitiveLocation
import SimpleDB.VersionData
import SimpleDB.BDBOps(BDBKey)

lastChange :: SimpleDB -> ObjectVersion -> Location -> IO ObjectVersion
lastChange simpleDB thisVersion location =
   do
      let
         getKey :: VersionData -> IO BDBKey
         getKey versionData =
            do
               let
                  pLocation = retrievePrimitiveLocation versionData location
               retrieveKey versionData pLocation

      thisVersionData <- getVersionData simpleDB thisVersion
      thisKey <- getKey thisVersionData
      let
         search :: ObjectVersion -> VersionData -> IO ObjectVersion
         search thisVersion thisVersionData = case parent thisVersionData of
            Nothing -> return thisVersion
            Just parentVersion ->
               do
                  parentVersionData <- getVersionData simpleDB parentVersion
                  parentKey <- getKey parentVersionData
                  if parentKey == thisKey
                     then
                        search parentVersion parentVersionData
                     else
                        return thisVersion
      search thisVersion thisVersionData

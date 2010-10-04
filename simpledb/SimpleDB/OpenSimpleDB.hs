-- | Initialise the server and so create the `SimpleDB` type.
module SimpleDB.OpenSimpleDB(
   openSimpleDB,
   ) where

import Data.IORef
import qualified Data.Map as Map

import Util.Computation

import SimpleDB.Types
import SimpleDB.VersionData
import SimpleDB.BDBOps
import SimpleDB.BDBExtras
import SimpleDB.VersionInfo
import SimpleDB.Permissions
import SimpleDB.LocationAllocation
import SimpleDB.VersionAllocation
import SimpleDB.VersionState

-- -------------------------------------------------------------------
-- Creating a new SimpleDB
-- -------------------------------------------------------------------

-- | Start the server, if necessary recovering the data stored on previous
-- runs.
openSimpleDB :: VersionState -> IO SimpleDB
openSimpleDB versionState =
   do
      -- (1) connect to BDB databases
      miscDB <- openBDB "Misc"
      versionDB <- openBDB "Version"
      securityDB <- openBDB "Security"
      keyDB <- openBDB "Key"
      dataDB <- openBDB "Data"

      let
         -- Create a preliminary version, containing just the
         -- databases, to base to initSimpleDB.
         simpleDB1 = SimpleDB {
            miscDB = miscDB,
            versionDB = versionDB,
            securityDB = securityDB,
            keyDB = keyDB,
            dataDB = dataDB,

            -- fill in extra fields to avoid irritating GHC warning.
            versionData = error "1",
            openVersions = error "2",
            openLocations = error "2A",
            versionState = error "3",
            nextLocation = error "4",
            nextVersion = error "5"
            }

      -- (2) Do the initialisations which need to be done if the
      -- database is being created for the very first time.
      initSimpleDB simpleDB1

      -- (3) read in the initial versionData.
      versionData1 <- createVersionData versionDB
      versionData <- newIORef versionData1

      -- (4) get the nextLocation and nextVersion data.
      nextLocation <- initLocations miscDB
      nextVersion <- initVersions miscDB

      -- (5) create lists of open versions and open locations
      openVersions <- newIORef Map.empty
      openLocations <- newIORef Map.empty

      let
         simpleDB = simpleDB1 {
            versionData = versionData,
            openVersions = openVersions,
            openLocations = openLocations,
            versionState = versionState,
            nextLocation = nextLocation,
            nextVersion = nextVersion
            }

      return simpleDB

-- | Intialise the information
initSimpleDB :: SimpleDB -> IO ()
initSimpleDB simpleDB =
   do
      (locationOpt :: Maybe Location) <- getObjectOpt (miscDB simpleDB) 1
      case locationOpt of
         Just location -> done -- database is already initialised
         Nothing ->
            do
               -- Write all special locations
               txn <- beginTransaction

               setObjectHere1 (miscDB simpleDB) 1 txn firstVersion
               setObjectHere1 (miscDB simpleDB) 2 txn (succ specialLocation2)
               setObjectHere1 (miscDB simpleDB) 3 txn ([] :: Permissions)

               setObjectHere1 (securityDB simpleDB) 1 txn ([] :: Permissions)
               setObjectHere1 (securityDB simpleDB) 2 txn ([] :: Permissions)
               endTransaction txn
               flushBDB (securityDB simpleDB)
               flushBDB (miscDB simpleDB)

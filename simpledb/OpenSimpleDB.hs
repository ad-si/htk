-- | Initialise the server and so create the `SimpleDB` type.
module OpenSimpleDB(
   openSimpleDB,
   ) where

import Data.IORef
import Data.FiniteMap

import Computation

import SimpleDBTypes
import VersionData
import BDBOps
import BDBExtras
import VersionInfo
import Permissions
import SecurityManagement
import LocationAllocation
import VersionAllocation
import VersionState

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

      -- (5) create list of open versions
      openVersions <- newIORef emptyFM

      let
         simpleDB = simpleDB1 {
            versionData = versionData,
            openVersions = openVersions,
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

               setObjectHere1 (miscDB simpleDB) 0 txn firstVersion
               setObjectHere1 (miscDB simpleDB) 1 txn (succ specialLocation2)
               setObjectHere1 (miscDB simpleDB) 2 txn ([] :: Permissions)

               setObjectHere1 (securityDB simpleDB) 0 txn 
                  (trivialSecurityData Nothing)
               setObjectHere1 (securityDB simpleDB) 1 txn
                  (trivialSecurityData Nothing)
               endTransaction txn
               flushBDB (securityDB simpleDB)
               flushBDB (miscDB simpleDB)

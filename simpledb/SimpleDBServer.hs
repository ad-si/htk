{- This module introduces the functions available to the server to access
   the database.  

   NB.  At various points we assume that these functions are only used
   in a single-threaded way.  The server code should ensure this. -}
module SimpleDBServer(
   SimpleDB, -- SimpleDB.  There should be only one of them per program.
   openSimpleDB, -- :: IO SimpleDB
      -- Initialise database, reading backup off disk if necessary.

   -- SimpleDBCommand/Response are the types of queries and responses
   -- to the DB.
   -- Each is an instance of Read and Show.
   SimpleDBCommand(..),
   SimpleDBResponse(..),

   -- Location of objects.
   Location,
   -- The specialLocation1 and specialLocation2 are preallocated locations,
   -- they will never be allocated by newLocation.
   specialLocation1, -- :: Location
   specialLocation2, -- :: Location

   -- Type of versions.  These versions are global, that is they refer to
   -- collections of objects, not individual objects.
   ObjectVersion,
   -- firstVersion points to the first version that will be
   -- allocated.
   firstVersion, -- :: Version

   querySimpleDB, -- :: SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
   ) where

import IO
import Maybe

import Data.FiniteMap

import Computation
import ExtendedPrelude
import AtomString
import IOExts
import Dynamics
import FileNames
import ICStringLen
import BinaryIO

import CacheTable
import IOExtras
import WBFiles

import BDBClient

-- -------------------------------------------------------------------
-- The query types.
-- -------------------------------------------------------------------

newtype Location = Location Int deriving (Eq,Ord,HasBinaryIO)

specialLocation1 :: Location
specialLocation1 = Location 0

specialLocation2 :: Location
specialLocation2 = Location 1

maxLocation :: Location 
maxLocation = specialLocation2 
   -- this should be the highest initially allocated location.

newtype ObjectVersion = ObjectVersion Int deriving (Eq,Ord,HasBinaryIO)

firstVersion :: ObjectVersion
firstVersion = ObjectVersion 0

maxVersion :: ObjectVersion 
maxVersion = ObjectVersion (-1)
   -- this should be the highest initially allocated version.
   -- Currently no version is initially allocated.

data SimpleDBCommand =
   -- All commands may additionally return IsError.

      NewLocation -- returns a new location (IsLocation),
   |  NewVersion -- a new object version (IsObjectVersion).
   |  ListVersions -- returns list of all known objects
         -- return with IsObjectVersions
   |  Retrieve Location ObjectVersion
         -- returns IsData .
   |  LastChange Location ObjectVersion
         -- Return the ObjectVersion (IsObjectVersion) in which this
         -- object (Location inside ObjectVersion) was last changed,
         -- or IsResponse with an error message.
         -- retrieve version ObjectVersion from the repository
         -- return IsData.
   |  Commit (Maybe ObjectVersion) ObjectVersion [(Location,ICStringLen)]
         -- The first argument is a parent version, if any, which should
         -- be something already committed.  The second is the ObjectVersion
         -- which should be uniquely allocated for this ObjectVersion;
         -- this can be firstVersion (when we are initialising the
         -- repository) or else something allocated by NewVersion.
         --
         -- Returns IsOK.
         --
         -- It is in fact legitimate for the same Location to occur more
         -- than once in the list (this will be necessary when we do security
         -- properly).  All but the last occurrence is discarded.
   |  MultiCommand [SimpleDBCommand]
         -- A group of commands to be executed one after another.
         -- Returns MultiResponse with the corresponding responses.
#ifdef DEBUG
   deriving (Show)
#endif

data SimpleDBResponse =
      IsLocation Location
   |  IsObjectVersion ObjectVersion
   |  IsObjectVersions [ObjectVersion]
   |  IsData ICStringLen
   |  IsError String
   |  IsOK
   |  MultiResponse [SimpleDBResponse]
#ifdef DEBUG
   deriving (Show)
#endif

-- -----------------------------------------------------------------------
-- SimpleDBCommand/Response as instances of HasBinaryIO
-- -----------------------------------------------------------------------

instance HasWrapper SimpleDBCommand where
   wraps = [
      wrap0 'n' NewLocation,
      wrap0 'N' NewVersion,
      wrap0 'L' ListVersions,
      wrap2 'R' Retrieve,
      wrap2 'c' LastChange,
      wrap3 'C' Commit,
      wrap1 'M' MultiCommand
      ]
   unWrap = (\ wrapper -> case wrapper of
      NewLocation -> UnWrap 'n' ()
      NewVersion -> UnWrap 'N' ()
      ListVersions -> UnWrap 'L' ()
      Retrieve l v -> UnWrap 'R' (l,v)
      LastChange l v -> UnWrap 'c' (l,v)
      Commit p v n -> UnWrap 'C' (p,v,n)
      MultiCommand l -> UnWrap 'M' l
      )

instance HasWrapper SimpleDBResponse where
   wraps = [
      wrap1 'L' IsLocation,
      wrap1 'o' IsObjectVersion,
      wrap1 'O' IsObjectVersions,
      wrap1 'D' IsData,
      wrap1 'E' IsError,
      wrap0 'K' IsOK,
      wrap1 'M' MultiResponse
      ]
   unWrap = (\ wrapper -> case wrapper of
      IsLocation l -> UnWrap 'L' l
      IsObjectVersion v -> UnWrap 'o' v
      IsObjectVersions vs -> UnWrap 'O' vs
      IsData d -> UnWrap 'D' d
      IsError e -> UnWrap 'E' e
      IsOK -> UnWrap 'K' ()
      MultiResponse l -> UnWrap 'M' l
      )

-- -------------------------------------------------------------------
-- Instances for locations and objectVersions
-- -------------------------------------------------------------------

instance StringClass Location where
   toString (Location i) = show i
   fromString s = Location (read s)

location_tyRep = mkTyRep "SimpleDBServer" "Location"
instance HasTyRep Location where
   tyRep _ = location_tyRep

instance StringClass ObjectVersion where
   toString (ObjectVersion i) = show i
   fromString s = ObjectVersion (read s)

objectVersion_tyRep = mkTyRep "SimpleDBServer" "ObjectVersion"
instance HasTyRep ObjectVersion where
   tyRep _ = objectVersion_tyRep

-- -------------------------------------------------------------------
-- The SimpleDB type.
-- -------------------------------------------------------------------

data SimpleDB = SimpleDB {
   -- objectLocations is a backup file we write all details to.
   objectLocations :: Handle,
   -- details for each version in the db.
   versionDictionary :: IORef (FiniteMap ObjectVersion VersionData),
   -- Next location to allocate
   nextLocation :: IORef Int,
   -- Next version to allocate.
   nextVersion :: IORef Int,
   -- The BDB Database
   bdb :: BDB
   }

data VersionData = VersionData {
   parent :: Maybe ObjectVersion,
   thisVersion :: ObjectVersion,
   objectDictionary :: FiniteMap Location BDBKey
      -- The dictionary contains every location in the ObjectDictionary,
      -- even those which are identical to those in the parent.  However
      -- we construct the objectDictionary starting with the dictionary
      -- for the parent and adding the changes in this version.  Thus,
      -- because of persistence, the actual extra memory occupied on the
      -- server should be small, if there are only a few changes from the
      -- parent.      
   }

-- FrozenVersionData corresponds to the data received by a Commit command,
-- or else to a single entry in the backup file.
data FrozenVersionData = FrozenVersionData {
   parent' :: Maybe ObjectVersion,
   thisVersion' :: ObjectVersion,
   objectChanges :: [(Location,BDBKey)]
   }

-- -------------------------------------------------------------------
-- The main query function
-- -------------------------------------------------------------------

querySimpleDB :: SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB
      (simpleDB @ (SimpleDB {
         objectLocations = objectLocations,
         nextLocation = nextLocation,
         nextVersion = nextVersion,
         versionDictionary = versionDictionary,
         bdb = bdb
         }))
      command =
   case command of
      NewLocation ->
         do
            loc <- readIORef nextLocation
            writeIORef nextLocation (loc + 1)
            return (IsLocation (Location loc))
      NewVersion ->
         do
            version <- readIORef nextVersion
            writeIORef nextVersion (version + 1)
            return (IsObjectVersion (ObjectVersion version))
      LastChange location objectVersion ->
         do
            versionWE <- lastChange simpleDB location objectVersion
            return (case fromWithError versionWE of
               Left mess -> IsError mess
               Right version -> IsObjectVersion version
               )
      ListVersions ->
         do
            versionMap <- readIORef versionDictionary
            return (IsObjectVersions (keysFM versionMap))
      Retrieve location objectVersion ->
         do
            bdbKeyWE <- retrieveKey simpleDB location objectVersion
            case fromWithError bdbKeyWE of
               Left mess -> return (IsError mess)
               Right bdbKey ->
                  do
                     icslOpt <- readBDB bdb bdbKey
                     case icslOpt of
                        Nothing -> return (IsError (
                           "Retrieve failed: bdbkey not saved.  "
                           ++ "\n Perhaps the server crashed half-way "
                           ++ "\n through a commit?"))
                        Just icsl -> return (IsData icsl)
      Commit parentVersionOpt thisVersion newStuff0 ->
         do
            -- enter the new stuff in the BDB repository
            newStuff1 <- mapM
               (\ (location,icsl) ->
                  do
                     bdbKey <- writeBDB bdb icsl
                     return (location,bdbKey)
                  )
               newStuff0

            flushBDB bdb

            -- construct a FrozenVersionData
            let
               frozenVersionData = FrozenVersionData {
                  parent' = parentVersionOpt,
                  thisVersion' = thisVersion,
                  objectChanges = newStuff1
                  }

             -- apply it
            unitWE <- applyFrozenVersionData simpleDB frozenVersionData
            return (
               case fromWithError unitWE of
                  Right () -> IsOK
                  Left mess -> IsError mess
               )
      MultiCommand commands ->
         do
            responses <- mapM (querySimpleDB simpleDB) commands
            return (MultiResponse responses)

-- -------------------------------------------------------------------
-- Functions for retrieving data from the repository
-- -------------------------------------------------------------------

retrieveKeyOpt :: SimpleDB -> Location -> ObjectVersion 
   -> IO (WithError (Maybe BDBKey))
retrieveKeyOpt simpleDB location objectVersion =
   do
      versionMap <- readIORef (versionDictionary simpleDB)
      return (case lookupFM versionMap objectVersion of
         Nothing -> 
            hasError "Retrieve failed; version does not exist!"
         Just versionData ->
            hasValue (lookupFM (objectDictionary versionData) location)
         )

retrieveKey :: SimpleDB -> Location -> ObjectVersion -> IO (WithError BDBKey)
retrieveKey simpleDB location objectVersion =
   do
      bdbKeyOptWE <- retrieveKeyOpt simpleDB location objectVersion
      return (mapWithError' 
         (\ bdbKeyOpt -> case bdbKeyOpt of
            Nothing -> hasError "Retrieve failed; location does not exist!"
            Just bdbKey -> hasValue bdbKey
            )
         bdbKeyOptWE
         )  

retrieveParent :: SimpleDB -> ObjectVersion 
   -> IO (WithError (Maybe ObjectVersion))
retrieveParent simpleDB objectVersion =
   do
      versionMap <- readIORef (versionDictionary simpleDB)
      case lookupFM versionMap objectVersion of
         Nothing -> return (hasError
            "Parent lookup failed failed; version does not exist!"
            )
         Just versionData -> return (hasValue (parent versionData))

lastChange :: SimpleDB -> Location -> ObjectVersion 
   -> IO (WithError ObjectVersion)
lastChange simpleDB location objectVersion =
   do
      thisKeyWE <- retrieveKey simpleDB location objectVersion
      let
         search :: BDBKey -> ObjectVersion -> IO (WithError ObjectVersion)
         search lastKey lastVersion =
            do
               parentOptWE <- retrieveParent simpleDB lastVersion
               mapWithErrorIO'
                  (\ parentOpt -> case parentOpt of
                     Nothing -> return (hasValue lastVersion)
                     Just parentVersion ->
                        do
                           parentKeyOptWE <- 
                              retrieveKeyOpt simpleDB location parentVersion
                           mapWithErrorIO'
                              (\ parentKeyOpt -> case parentKeyOpt of
                                 Just parentKey | parentKey == lastKey ->
                                    search parentKey parentVersion
                                 _ -> return (hasValue lastVersion)
                                 )
                              parentKeyOptWE
                     )
                  parentOptWE

      mapWithErrorIO'
         (\ thisKey -> search thisKey objectVersion)
         thisKeyWE
        
      

-- -------------------------------------------------------------------
-- Setting the next Location and ObjectVersion.
-- -------------------------------------------------------------------

setNextThings :: SimpleDB -> IO ()
setNextThings simpleDB =
   do
      objectVersions <- allObjectVersions simpleDB
      let
         (ObjectVersion ov) = maximum (maxVersion : objectVersions)
      writeIORef (nextVersion simpleDB) (ov + 1)

      locations <- allLocations simpleDB
      let
         (Location l) = maximum (maxLocation : locations)
      writeIORef (nextLocation simpleDB) (l + 1)


allObjectVersions :: SimpleDB -> IO [ObjectVersion]
allObjectVersions simpleDB =
   do
      versionMap <- readIORef (versionDictionary simpleDB)
      return (keysFM versionMap)

allLocations :: SimpleDB -> IO [Location]
allLocations simpleDB =
   do
      versionMap <- readIORef (versionDictionary simpleDB)
      let
         versionDatas = eltsFM versionMap
         locations = concat (
            map
               (\ versionData -> keysFM (objectDictionary versionData))
               versionDatas
            )
      return locations



-- -------------------------------------------------------------------
-- FrozenVersionData operations
-- -------------------------------------------------------------------

instance HasBinaryIO FrozenVersionData where
   hPut = mapHPut (\ (
      FrozenVersionData {
         parent' = parent',thisVersion' = thisVersion',
         objectChanges = objectChanges
         }
      ) -> 
      (parent',thisVersion',objectChanges)
      )
   hGetIntWE = mapHGetIntWE (\ (
      (parent',thisVersion',objectChanges)
      ) ->
      FrozenVersionData {
         parent' = parent',thisVersion' = thisVersion',
         objectChanges = objectChanges
         }
      )

-- Adding new FrozenVersionData to a SimpleDB, without updating the
-- nextLocation/nextVersion.  
addFrozenVersionData :: SimpleDB -> FrozenVersionData -> IO (WithError ())
addFrozenVersionData simpleDB frozenVersionData =
   addFallOutWE (\ break ->
      do
         let
            vDictRef = versionDictionary simpleDB
         vDict0 <- readIORef vDictRef

         let
            thisVersion = thisVersion' frozenVersionData
            parent = parent' frozenVersionData
 
         case lookupFM vDict0 thisVersion of
            Nothing -> done
            Just _ -> break "Version already exists on server"

         let
            parentDictionary = case parent of
               Nothing -> emptyFM
               Just par -> case lookupFM vDict0 par of
                  Just parentData -> objectDictionary parentData
                  Nothing -> break "Parent version does not exist on server"
         seq parentDictionary done

         let
            objectDictionary =
               foldl
                  (\ map0 (location,bdbKey) -> addToFM map0 location bdbKey)
                  parentDictionary
                  (objectChanges frozenVersionData)

            versionData = VersionData {
               parent = parent,
               thisVersion = thisVersion,
               objectDictionary = objectDictionary
               }

            vDict1 = addToFM vDict0 thisVersion versionData

         writeIORef vDictRef vDict1
      )

-- Reading FrozenVersionData from a file
readFrozenVersionDatas :: Handle -> IO [FrozenVersionData]
readFrozenVersionDatas handle =
   do
      pos1 <- hGetPosn handle
      frozenVersionDataWEOpt <- catchEOF (hGetWE handle)
      case frozenVersionDataWEOpt of
         Nothing -> -- EOF
            do
               pos2 <- hGetPosn handle
               unless (pos1 == pos2)
                  (do 
                     putStrLn 
                        "Restarting server: incomplete commit discarded"
                     hSetPosn pos1
                  )
               return [] -- this is how
         Just frozenVersionDataWE -> 
            case fromWithError frozenVersionDataWE of
               Left mess ->
                  error (
                     "Server could not be restarted due to error reading "
                     ++ "directory file: " ++ mess)
               Right frozenVersionData ->
                  do
                     frozenVersionDatas <- readFrozenVersionDatas handle
                     return (frozenVersionData : frozenVersionDatas)
         
-- -------------------------------------------------------------------
-- Creating a new SimpleDB
-- -------------------------------------------------------------------

openSimpleDB :: IO SimpleDB
openSimpleDB =
   do
      -- (1) connect to BDB database
      bdb <- openBDB

      -- (2) open the object locations file.
      fpath <- getServerFile "objectLocs"
      handle <- openFile fpath ReadWriteMode

      -- (3) create an empty SimpleDB
      versionDictionary <- newIORef emptyFM
      nextLocation <- newIORef 0
      nextVersion <- newIORef 0

      let
         simpleDB = SimpleDB {
            objectLocations = handle,
            versionDictionary = versionDictionary,
            nextLocation = nextLocation,
            nextVersion = nextVersion,
            bdb = bdb
            }

      -- (4) get the frozenVersionDatas
      frozenVersionDatas <- readFrozenVersionDatas handle

      -- (5) add them to the simpleDB
      mapM_
         (\ frozenVersionData -> 
            do
               unitWE <- addFrozenVersionData simpleDB frozenVersionData
               coerceWithErrorIO unitWE
            )
         frozenVersionDatas

      -- (6) compute the nextLocation and nextVersion
      setNextThings simpleDB

      return simpleDB

-- -------------------------------------------------------------------
-- Apply a particular FrozenVersionData obtained from a client, writing
-- it to a backup file.
-- -------------------------------------------------------------------

applyFrozenVersionData :: SimpleDB -> FrozenVersionData -> IO (WithError ())
applyFrozenVersionData simpleDB frozenVersionData =
   do
      -- (1) add to SimpleDB.
      unitWE <- addFrozenVersionData simpleDB frozenVersionData
      -- (2) write out
      mapWithErrorIO
         (\ () ->
            do
               let
                  handle = objectLocations simpleDB
               hPut handle frozenVersionData
               hFlush handle
            )
         unitWE



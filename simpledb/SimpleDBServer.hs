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
   -- Each is an instance of Show.
   SimpleDBCommand(..),
   SimpleDBResponse(..),

   Diff(..),
      -- used in SimpleDBResponse to encode the difference between a version
      -- and (presumably, earlier) versions.

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
import Monad

import Data.FiniteMap
import Data.Set

import Computation
import ExtendedPrelude
import AtomString
import IOExts
import Dynamics
import FileNames
import ICStringLen
import BinaryIO

import IOExtras
import WBFiles

import PasswordFile
import LogFile

import BDBClient
import VersionInfo

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
   |  Commit 
         (Either UserInfo VersionInfo) 
             -- ^ extra data about this version.
             -- This includes a version number, which must be uniquely
             -- allocated by NewVersion, and the parent versions, which
             -- must already exist.  The first parent version we call the
             -- head parent version.
         [(Location,Either ICStringLen (Location,ObjectVersion))]
             -- ^ The new stuff in this version, as compared with the head
             -- parent version, by Location.
         -- Normally, the list items will be Left ICStringLen, for new data. 
         -- But because of merges, we also need 
         -- Right (Location,ObjectVersion), which indicates a pre-existing 
         -- item in the database given by this location and object version.
         --
         -- Returns IsOK.
         --
         -- It is in fact legitimate for the same Location to occur more
         -- than once in the list (this will be necessary when we do security
         -- properly).  All but the last occurrence is discarded.
   |  ModifyUserInfo UserInfo
         -- For a version which already exists, replace its VersionInfo
         -- by that given. 
         --    This will not change the head parent version on any account.
         -- Returns IsOK.
   |  GetDiffs ObjectVersion [ObjectVersion]
         -- Produce a list of changes between the given object version
         -- and the parents, in the format IsDiffs.
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
   |  IsDiffs [(Location,Diff)]
   |  IsError String
   |  IsOK
   |  MultiResponse [SimpleDBResponse]
#ifdef DEBUG
   deriving (Show)
#endif

data Diff = 
   -- returned from GetDiffs command.  
   -- The "parent versions" are the versions in the second argument of
   --    GetDiffs.
   -- The "parent version" is the first element of this list (if any).
      IsOld -- Location exists in parent version, and is unchanged.
   |  IsChanged {changed :: Maybe (Location,ObjectVersion)}
         -- Location exists in one of the parent versions, but has been
         -- changed.
   |  IsNew {changed :: Maybe (Location,ObjectVersion)}
         -- Location exists in none of the parent versions.
   -- If changed is Just (location,objectVersion) then
   -- "objectVersion" is a parentVersion, and the contents in the subject
   -- version are identical with those in (location,objectVersion);
   -- indeed the contents are not just byte-for-byte identical, but
   -- can be deduced to be identical from the rules that 
   -- (a) locations in views created by "Commit" are unchanged unless
   --     otherwise specified;
   -- (b) locations whose contents are specified as (location,version1)
   --     on commit have identical contents to those of (location,version1).
#ifdef DEBUG
   deriving (Show)
#endif

-- -----------------------------------------------------------------------
-- SimpleDBCommand/Response & Diff as instances of HasBinaryIO
-- -----------------------------------------------------------------------

instance HasWrapper SimpleDBCommand where
   wraps = [
      wrap0 'n' NewLocation,
      wrap0 'N' NewVersion,
      wrap0 'L' ListVersions,
      wrap2 'R' Retrieve,
      wrap2 'c' LastChange,
      wrap2 'C' Commit,
      wrap1 'm' ModifyUserInfo,
      wrap2 'd' GetDiffs,
      wrap1 'M' MultiCommand
      ]
   unWrap = (\ wrapper -> case wrapper of
      NewLocation -> UnWrap 'n' ()
      NewVersion -> UnWrap 'N' ()
      ListVersions -> UnWrap 'L' ()
      Retrieve l v -> UnWrap 'R' (l,v)
      LastChange l v -> UnWrap 'c' (l,v)
      Commit v n -> UnWrap 'C' (v,n)
      ModifyUserInfo v -> UnWrap 'm' v
      GetDiffs v vs -> UnWrap 'd' (v,vs)
      MultiCommand l -> UnWrap 'M' l
      )

instance HasWrapper SimpleDBResponse where
   wraps = [
      wrap1 'L' IsLocation,
      wrap1 'o' IsObjectVersion,
      wrap1 'O' IsObjectVersions,
      wrap1 'D' IsData,
      wrap1 'E' IsError,
      wrap1 'd' IsDiffs,
      wrap0 'K' IsOK,
      wrap1 'M' MultiResponse
      ]
   unWrap = (\ wrapper -> case wrapper of
      IsLocation l -> UnWrap 'L' l
      IsObjectVersion v -> UnWrap 'o' v
      IsObjectVersions vs -> UnWrap 'O' vs
      IsData d -> UnWrap 'D' d
      IsDiffs ds -> UnWrap 'd' ds
      IsError e -> UnWrap 'E' e
      IsOK -> UnWrap 'K' ()
      MultiResponse l -> UnWrap 'M' l
      )

instance HasWrapper Diff where
   wraps = [
      wrap0 'O' IsOld,
      wrap1 'C' IsChanged,
      wrap1 'N' IsNew
      ]

   unWrap = (\ wrapper -> case wrapper of
      IsOld -> UnWrap 'O' ()
      IsChanged c -> UnWrap 'C' c
      IsNew c -> UnWrap 'N' c
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
   objectLocations :: LogFile FrozenVersionData,
   -- details for each version in the db.
   versionDictionary :: IORef (FiniteMap ObjectVersion VersionData),
   -- Next location to allocate
   nextLocation :: IORef Int,
   -- Next version to allocate.
   nextVersion :: IORef Int,
   -- The BDB Database
   bdb :: BDB,
   -- The additional version information
   versionState :: VersionState
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
   objectChanges :: [(Location,Either BDBKey (Location,ObjectVersion))]
   }

-- -------------------------------------------------------------------
-- The main query function
-- -------------------------------------------------------------------

querySimpleDB :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB user
      (simpleDB @ (SimpleDB {
         objectLocations = objectLocations,
         nextLocation = nextLocation,
         nextVersion = nextVersion,
         versionDictionary = versionDictionary,
         versionState = versionState,
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
      Commit versionExtra newStuff0 ->
         do
            versionInfoWE <- mkVersionInfo user versionExtra
            case fromWithError versionInfoWE of
               Left mess -> return (IsError mess)
               Right versionInfo ->
                  do
                     let
                        thisVersion = version (VersionInfo.user versionInfo)

                        parentVersionOpt = 
                           case parents (VersionInfo.user versionInfo) of
                              [] -> Nothing
                              head : _ -> Just head

                     txn <- beginTransaction

                     -- enter the new stuff in the BDB repository 
                     (newStuff1 :: [(Location,
                              Either BDBKey (Location,ObjectVersion))])
                           <- mapM
                        (\ (location,newItem) ->
                           case newItem of
                              Left icsl ->
                                 do
                                    bdbKey <- writeBDB bdb txn icsl
                                    return (location,Left bdbKey)
                              Right objectLoc 
                                 -> return (location,Right objectLoc)
                           )
                        newStuff0

                     -- construct a FrozenVersionData
                     let
                        frozenVersionData = FrozenVersionData {
                           parent' = parentVersionOpt,
                           thisVersion' = thisVersion,
                           objectChanges = newStuff1
                           }

                      -- apply it
                     unitWE 
                        <- applyFrozenVersionData simpleDB frozenVersionData
                     
                     case fromWithError unitWE of
                        Right () -> 
                           do
                              endTransaction txn

                              flushBDB bdb
                                 -- not sure if this is necessary, but it won't
                                 -- hurt.

                              -- transmit extra version data
                              addVersionInfo versionState (False,versionInfo)
                              return IsOK
                        Left mess -> 
                           do
                              abortTransaction txn
                              return (IsError mess)
      ModifyUserInfo userInfo ->
         do
            oldVersionInfoOpt 
               <- lookupVersionInfo versionState (version userInfo)
            case oldVersionInfoOpt of
               Nothing -> return (IsError 
                  "ModifyUserInfo: Version does not exist")
               Just versionInfo0 ->
                  do
                     let
                        versionInfo1WE 
                           = changeUserInfo user versionInfo0 userInfo
                     case fromWithError versionInfo1WE of
                        Right versionInfo1 ->
                           do
                              addVersionInfo versionState (True,versionInfo1)
                              return IsOK
                        Left mess -> return (IsError mess)
      GetDiffs version versions ->
         do
            diffsWE <- getDiffs simpleDB version versions
            return (case fromWithError diffsWE of
               Left mess -> IsError mess
               Right diffs -> IsDiffs diffs
               )
      MultiCommand commands ->
         do
            responses <- mapM (querySimpleDB user simpleDB) commands
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

         objectDictionary <-
            foldM
               (\ map0 (location,change) ->
                  do
                     bdbKey <- case change of
                        Left bdbKey -> return bdbKey
                        Right (location,objectVersion) ->
                           do
                              bdbKeyWE 
                                 <- retrieveKey simpleDB location objectVersion
                              bdbKey <- coerceWithErrorOrBreakIO break bdbKeyWE
                              return bdbKey
                     return (addToFM map0 location bdbKey)
                  )
               parentDictionary
               (objectChanges frozenVersionData)

         let
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
               return [] -- this is how we normally end.
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

openSimpleDB :: VersionState -> IO SimpleDB
openSimpleDB versionState =
   do
      -- (1) connect to BDB database
      bdb <- openBDB

      -- (2) open the object locations file.
      (objectLocations,frozenVersionDatas) <- openLog "objectLocs"

      -- (3) create an empty SimpleDB
      versionDictionary <- newIORef emptyFM
      nextLocation <- newIORef 0
      nextVersion <- newIORef 0

      let
         simpleDB = SimpleDB {
            objectLocations = objectLocations,
            versionDictionary = versionDictionary,
            nextLocation = nextLocation,
            nextVersion = nextVersion,
            bdb = bdb,
            versionState = versionState
            }

      -- (4) add them to the simpleDB
      mapM_
         (\ frozenVersionData -> 
            do
               unitWE <- addFrozenVersionData simpleDB frozenVersionData
               coerceWithErrorIO unitWE
            )
         frozenVersionDatas

      -- (5) compute the nextLocation and nextVersion
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
            writeLog (objectLocations simpleDB) frozenVersionData
            )
         unitWE


-- -------------------------------------------------------------------
-- Extract differences
-- -------------------------------------------------------------------

getDiffs :: SimpleDB -> ObjectVersion -> [ObjectVersion] 
   -> IO (WithError [(Location,Diff)])
getDiffs db version0 versions = addFallOutWE (\ break ->
   do
      vDict <- readIORef (versionDictionary db)
      let
         lookup version = case lookupFM vDict version of
            Just versionData -> return versionData
            Nothing -> break ("Version "++show version++" not found")
      vData0 <- lookup version0
      vDatas <- mapM lookup versions
      return (case vDatas of
         [] -> -- we just have to return IsNew for everything
            map 
               (\ location -> (location,IsNew {changed = Nothing}))   
               (keysFM (objectDictionary vData0))
         (headVData :_) ->
            let
               -- Construct a map back from BDBKey -> (Location,ObjectVersion)
               -- for the parents.
               bdbDict :: FiniteMap BDBKey (Location,ObjectVersion)
               bdbDict = foldl
                  (\ map0 vData ->
                     let
                        version = thisVersion vData
                     in
                        foldFM
                           (\ location bdbKey map0 
                              -> addToFM map0 bdbKey (location,version)
                              )
                           map0
                           (objectDictionary vData)
                     ) 
                  emptyFM
                  vDatas

               -- Construct the set of all Locations in parent versions.
               locationSet :: Set Location
               locationSet = foldl
                  (\ set0 vData ->
                     foldFM
                        (\ location _ set0 -> addToSet set0 location)
                        set0
                        (objectDictionary vData)
                     )
                  emptySet
                  vDatas 

               -- Function constructing Diff for a particular item in the 
               -- new version's object dictionary
               mkDiff :: (Location,BDBKey) -> Diff
               mkDiff (location,key1) =
                  case lookupFM (objectDictionary headVData) location of
                     Just key2 | key1 == key2 
                        -> IsOld
                     _ -> 
                        let
                           changed = lookupFM bdbDict key1
                        in
                           if elementOf location locationSet
                              then
                                 IsNew {changed = changed}
                              else
                                 IsChanged {changed = changed}
            in
               map
                  (\ (lb @ (location,bdbKey)) -> (location,mkDiff lb))
                  (fmToList (objectDictionary vData0))
         )
   )
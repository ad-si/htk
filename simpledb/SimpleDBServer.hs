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
         [(Location,Maybe ObjectVersion)]
             -- Redirects.  This has the same format as the redirects'
             -- field in ServerOp.  For normal commits (not part of
             -- session management) it will be [].
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
      IsOld
         -- version exists in the parent version with this location and
         -- is unchanged.
   |  IsChanged {
         existsIn :: ObjectVersion,
         changed :: Maybe (Location,ObjectVersion)
         }
         -- Location exists in one of the parent versions, namely existsIn, 
         -- but has been changed.
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
      wrap3 'C' Commit,
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
      Commit v r n -> UnWrap 'C' (v,r,n)
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
      wrap2 'C' IsChanged,
      wrap1 'N' IsNew
      ]

   unWrap = (\ wrapper -> case wrapper of
      IsOld -> UnWrap 'O' ()
      IsChanged e c -> UnWrap 'C' (e,c)
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
   objectLocations :: LogFile ServerOp,
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

-- PrimitiveLocation's permit redirection of Locations.  Thus "Location"
-- is the user-visible Location, the PrimitiveLocation is the "real" one
-- as indexed by objectDictionary.
--
-- The purpose of this system is that the "Location" will remain constant,
-- even when versions are exported from one repository to another.  However
-- the repository is free to reassign PrimitiveLocation's behind the
-- scenes. 
newtype PrimitiveLocation = PrimitiveLocation Int deriving (Eq,Ord,HasBinaryIO)



data VersionData = VersionData {
   parent :: Maybe ObjectVersion,
   thisVersion :: ObjectVersion,
   objectDictionary :: FiniteMap PrimitiveLocation BDBKey,
      -- The dictionary contains every location in the ObjectDictionary,
      -- even those which are identical to those in the parent.  However
      -- we construct the objectDictionary starting with the dictionary
      -- for the parent and adding the changes in this version.  Thus,
      -- because of persistence, the actual extra memory occupied on the
      -- server should be small, if there are only a few changes from the
      -- parent.      
   redirects :: FiniteMap Location PrimitiveLocation
      -- This maps Location to the corresponding PrimitiveLocation,
      -- when the integers inside are different.
      -- As with objectDictionary, we use persistence.
   }

-- ServerOp corresponds to a server operation which changes the state of the
-- server.  ServerOp's are written to the object locs file, so that they
-- can be read when the server restarts.
data ServerOp = 
   -- a commit operation
   FrozenVersion { 
      parent' :: Maybe ObjectVersion,
      thisVersion' :: ObjectVersion,
      objectChanges :: [(Location,Either BDBKey (Location,ObjectVersion))],
         -- a BDBKey means completely new data.
          -- (Location,ObjectVersion) means it so happens this is exactly the
          --    contents of this object are the same as those in 
          --    (Location,ObjectVersion)
      redirects' :: [(Location,Maybe ObjectVersion)]
          -- this gives redirects.  Note that this list does not have to 
          -- contain locations which are the same as the corresponding 
          -- primitive location, or where a redirect for this location already
          -- exists in the parent version.  
          --    The interpretation is as follows.  For (Just objectVersion)
          --    that means we copy whatever redirect exists, if any, for
          --    the same location in objectVersion.
          --    
          --    For Nothing, that means that this Location does not have an
          --    assigned PrimitiveLocation.  One should be created anew,
          --    and this Location mapped to it.
      }
   |  AllocVersion -- allocate a new version
   |  AllocLocation -- allocate a new Location.

-- -------------------------------------------------------------------
-- The main query function
-- -------------------------------------------------------------------

querySimpleDB :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB user
      (simpleDB @ (SimpleDB {
         objectLocations = objectLocations,
         versionDictionary = versionDictionary,
         versionState = versionState,
         bdb = bdb
         }))
      command =
   case command of
      NewLocation ->
         do
            locOptWE <- applyServerOp simpleDB AllocLocation
            case fromWithError locOptWE of
               Right (Just loc) -> return (IsLocation (Location loc))
      NewVersion ->
         do
            versOptWE <- applyServerOp simpleDB AllocVersion
            case fromWithError versOptWE of
               Right (Just vers) 
                  -> return (IsObjectVersion (ObjectVersion vers))
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
      Commit versionExtra redirects newStuff0 ->
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

                     -- construct a ServerOp
                     let
                        serverOp = FrozenVersion {
                           parent' = parentVersionOpt,
                           thisVersion' = thisVersion,
                           redirects' = redirects,
                           objectChanges = newStuff1
                           }

                      -- apply it
                     resWE <- applyServerOp simpleDB serverOp
                     
                     case fromWithError resWE of
                        Right Nothing -> 
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
            hasValue (retrieveKey1 versionData location)
         )

retrieveKey1 :: VersionData -> Location -> Maybe BDBKey
retrieveKey1 versionData location =
   let
      primitiveLocation = retrievePrimitiveLocation versionData location
   in
      lookupFM (objectDictionary versionData) primitiveLocation

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
-- ServerOp operations
-- -------------------------------------------------------------------

instance HasWrapper ServerOp where
   wraps = [
      wrap4 'C' FrozenVersion,
      wrap0 'V' AllocVersion,
      wrap0 'L' AllocLocation
      ]

   unWrap = (\ wrapper -> case wrapper of
      FrozenVersion p t o r -> UnWrap 'C' (p,t,o,r)
      AllocVersion -> UnWrap 'V' ()
      AllocLocation -> UnWrap 'L' ()
      )

-- Carry out a ServerOp, also writing it to the log file if successful.
applyServerOp :: SimpleDB -> ServerOp -> IO (WithError (Maybe Int))
applyServerOp simpleDB serverOp =
   do
      -- apply operation
      resultWE <- doServerOp simpleDB serverOp
      -- if successful write out
      case fromWithError resultWE of
         Left _ -> done
         Right _ -> writeLog (objectLocations simpleDB) serverOp
      return resultWE

-- Carry out a ServerOp, without writing anything to the log file.
-- Return () for a FrozenVersion and an integer for AllocVersion/AllocLocation.
-- NB.  An unsuccessful operation MUST NOT change the server's state,
-- as the log will not record it.  
doServerOp :: SimpleDB -> ServerOp -> IO (WithError (Maybe Int))
doServerOp simpleDB AllocVersion =
   do
      version <- getNextVersion simpleDB
      return (hasValue (Just version))
doServerOp simpleDB AllocLocation =
   do
      location <- getNextLocation simpleDB
      return (hasValue (Just location))
doServerOp simpleDB (
     FrozenVersion {
        parent' = parent,thisVersion' = thisVersion,
        objectChanges = objectChanges,redirects' = redirects'
        }) =
   addFallOutWE (\ break ->
      do
         let
            vDictRef = versionDictionary simpleDB
         vDict0 <- readIORef vDictRef

         case lookupFM vDict0 thisVersion of
            Nothing -> done
            Just _ -> break "Version already exists on server"

         let
            ((parentRedirects :: FiniteMap Location PrimitiveLocation),
             (parentDictionary :: FiniteMap PrimitiveLocation BDBKey)) 
               = case parent of
                     Nothing -> (emptyFM,emptyFM)
                     Just par -> case lookupFM vDict0 par of
                        Just parentData -> 
                           (redirects parentData,objectDictionary parentData)
                        Nothing -> break 
                           "Parent version does not exist on server"
         seq parentDictionary done
         
         oldVersion <- readIORef (nextVersion simpleDB)
         let
            break2 mess =
               do
                  writeIORef (nextVersion simpleDB) oldVersion
                  break mess

         -- compute redirects.
         redirects <- foldM
            (\ redirects0 (location,objectVersionOpt) ->
                  case objectVersionOpt of
                     Nothing ->
                        do
                           locNo <- getNextLocation simpleDB
                           return (addToFM redirects0 location
                              (PrimitiveLocation locNo))
                     Just objectVersion ->
                        case lookupFM vDict0 objectVersion of
                           Nothing -> break2 "Non-existent version in redirect"
                           Just versionData ->
                              let
                                 primLoc = retrievePrimitiveLocation 
                                    versionData location
                              in
                                 return (addToFM redirects0 location
                                       primLoc)
               )
            parentRedirects
            redirects'

         -- compute object dictionary
         objectDictionary <-
            foldM
               (\ map0 (location,change) ->
                  do
                     let 
                        primLocation 
                           = retrievePrimitiveLocation1 redirects location
                     bdbKey <- case change of
                        Left bdbKey -> return bdbKey
                        Right (location,objectVersion) ->
                           do
                              bdbKeyWE 
                                 <- retrieveKey simpleDB location objectVersion
                              bdbKey <- case fromWithError bdbKeyWE of
                                 Right bdbKey -> return bdbKey
                                 Left mess -> break2 mess
                              return bdbKey
                     return (addToFM map0 primLocation bdbKey)
                  )
               parentDictionary
               objectChanges

         let
            versionData = VersionData {
               parent = parent,
               thisVersion = thisVersion,
               objectDictionary = objectDictionary,
               redirects = redirects
               }

            vDict1 = addToFM vDict0 thisVersion versionData

         writeIORef vDictRef vDict1
         return Nothing
      )

getNextVersion :: SimpleDB -> IO Int
getNextVersion simpleDB = 
   simpleModifyIORef (nextVersion simpleDB)
      (\ version -> (version+1,version))

getNextLocation :: SimpleDB -> IO Int
getNextLocation simpleDB = 
   simpleModifyIORef (nextLocation simpleDB)
      (\ location -> (location+1,location))

-- -------------------------------------------------------------------
-- Creating a new SimpleDB
-- -------------------------------------------------------------------

openSimpleDB :: VersionState -> IO SimpleDB
openSimpleDB versionState =
   do
      -- (1) connect to BDB database
      bdb <- openBDB

      -- (2) open the object locations file.
      (objectLocations,serverOp) <- openLog "objectLocs"

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
         (\ serverOp -> 
            do
               unitWE <- doServerOp simpleDB serverOp
               coerceWithErrorIO unitWE
            )
         serverOp

      return simpleDB

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
               (getLocations vData0)
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
                        foldl
                           (\ map0 location ->
                              let
                                 Just bdbKey = retrieveKey1 vData location
                              in
                                 addToFM map0 bdbKey (location,version)
                              )
                           map0
                           (getLocations vData) 
                     )
                  emptyFM
                  vDatas

               -- Construct a map from locations to their containing parent
               -- version 
               locationMap :: FiniteMap Location ObjectVersion
               locationMap = foldl
                  (\ map0 vData ->
                     let
                        version = thisVersion vData
                     in
                        foldl
                           (\ map0 location -> addToFM map0 location version)
                           map0
                           (getLocations vData)
                     )
                  emptyFM
                  vDatas 

               -- Function constructing Diff for a particular item in the 
               -- new version's object dictionary
               mkDiff :: Location -> BDBKey -> Diff
               mkDiff location key1 =
                  case retrieveKey1 headVData location of
                     Just key2 | key1 == key2 
                        -> IsOld
                     _ -> 
                        let
                           changed = lookupFM bdbDict key1
                        in
                           case lookupFM locationMap location of
                              Nothing -> IsNew {changed = changed}
                              Just version ->
                                 IsChanged {existsIn = version,changed = changed}
            in
               map
                  (\ location ->
                     let
                        Just key = retrieveKey1 vData0 location
                     in
                        (location,mkDiff location key)
                     )
                  (getLocations vData0)
         )
   )
      
-- -------------------------------------------------------------------
-- Primitive location operations.
-- -------------------------------------------------------------------


getLocations :: VersionData -> [Location]
getLocations versionData =
   let
      redirectLocs = keysFM (redirects versionData)
      redirectPrimLocs = eltsFM (redirects versionData)

      objectDictMinusRedirects =
         foldl
            (\ map0 primLoc -> delFromFM map0 primLoc)
            (objectDictionary versionData)
            redirectPrimLocs

      otherPrimLocs = keysFM objectDictMinusRedirects
      otherLocs = map
         (\ (PrimitiveLocation locNo) -> Location locNo)
         otherPrimLocs
   in
      redirectLocs ++ otherLocs
      
retrievePrimitiveLocation :: VersionData -> Location -> PrimitiveLocation
retrievePrimitiveLocation versionData =
   retrievePrimitiveLocation1 (redirects versionData)

retrievePrimitiveLocation1 :: FiniteMap Location PrimitiveLocation 
    -> Location -> PrimitiveLocation
retrievePrimitiveLocation1 map (location @ (Location locNo)) =
   lookupWithDefaultFM map (PrimitiveLocation locNo) location



{- This module introduces the functions available to the server to access
   the database.  

   NB.  At various points we assume that these functions are only used
   in a single-threaded way.  The server code should ensure this. -}
module SimpleDBServer(
   SimpleDB, -- SimpleDB.  There should be only one of them per program.
   openSimpleDB, -- :: VersionState -> IO SimpleDB
      -- Initialise database, reading backup off disk if necessary.

   -- SimpleDBCommand/Response are the types of queries and responses
   -- to the DB.
   -- Each is an instance of Show.
   SimpleDBCommand(..),
   SimpleDBResponse(..),
   VersionInformation(..),

   ChangeData, -- = Either ICStringLen (Location,ObjectVersion)


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

   querySimpleDB, 
      -- :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse

   ServerOp, -- exported solely for MainDumpFiles.hs
   ) where

import IO
import Maybe
import Monad

import Control.Monad.Trans
import Data.FiniteMap
import Data.Set

import Computation
import ExtendedPrelude
import AtomString
import IOExts
import Dynamics
import FileNames
import ICStringLen
import BinaryAll

import IOExtras
import WBFiles

import PasswordFile
import LogFile

import BDBOps
import VersionInfo

-- -------------------------------------------------------------------
-- The query types.
-- -------------------------------------------------------------------

newtype Location = Location Int deriving (Eq,Ord,Show,Typeable)

specialLocation1 :: Location
specialLocation1 = Location 0

specialLocation2 :: Location
specialLocation2 = Location 1

data SimpleDBCommand =
   -- All commands may additionally return IsError.

      NewLocation -- returns a new location (IsLocation),
   |  NewVersion -- a new object version (IsObjectVersion).
   |  ListVersions -- returns list of all known objects
         -- return with IsObjectVersions
   |  Retrieve Location ObjectVersion
         -- returns IsData or IsNotFound.
   |  LastChange Location ObjectVersion
         -- Return the ObjectVersion (IsObjectVersion) in which this
         -- object (Location inside ObjectVersion) was last changed,
         -- or IsResponse with an error message.
         -- retrieve version ObjectVersion from the repository
         -- return IsData.
   |  Commit 
         VersionInformation
             -- extra data about this version.
             -- This includes a version number, which must be uniquely
             -- allocated by NewVersion, and the parent versions, which
             -- must already exist.  The first parent version we call the
             -- head parent version.
         [(Location,Maybe ObjectVersion)]
             -- Redirects.  This has the same format as the redirects'
             -- field in ServerOp.  For normal commits (not part of
             -- session management) it will be [].
         [(Location,ChangeData)]
             -- Returns IsOK, or if the operation could not
             -- be carried out because a version with the corresponding
             -- ServerInfo has already been checked in, returns 
             -- IsObjectVersion with the objectVersion of the old version.
   |  ModifyUserInfo VersionInformation
         -- If the version already exists, replace its VersionInfo by
         -- that supplied, assuming the permissions permit it.
         -- (to replace VersionInfo requires ADMIN permission or ownership
         -- of the version).
         -- If the version does not exist create it.  But the ObjectVersion
         -- should have been allocated by NewVersion.
         --    This will not change the head parent version on any account.
         --
         -- Additional restriction: If VersionInfo1 is used but the supplied 
         -- ServerInfo is already present in the database with a different
         -- versionNumber, we do nothing and return IsObjectVersion with
         -- the old version number. 
         -- Returns IsOK.
   |  GetVersionInfo ObjectVersion
         -- For a version which already exists, return its VersionInfo
         -- (as IsVersionInfo).
         -- If not found, return IsNotFound.
   |  GetDiffs ObjectVersion [ObjectVersion]
         -- Produce a list of changes between the given object version
         -- and the parents, in the format IsDiffs.
   |  MultiCommand [SimpleDBCommand]
         -- A group of commands to be executed one after another.
         -- Returns MultiResponse with the corresponding responses.
   deriving (Show)

data SimpleDBResponse =
      IsLocation Location
   |  IsObjectVersion ObjectVersion
   |  IsObjectVersions [ObjectVersion]
   |  IsData ICStringLen
   |  IsDiffs [(Location,Diff)]
   |  IsVersionInfo VersionInfo
   |  IsError String
   |  IsNotFound String
   |  IsOK
   |  MultiResponse [SimpleDBResponse]
  deriving (Show)

-- | Information about a Version sent on commit or ModifyUserInfo
data VersionInformation =
      UserInfo1 UserInfo 
         -- ^ information accessible to the user, such as the text description,
         -- plus the Version number.
   |  VersionInfo1 VersionInfo
         -- ^ everything we know about the version, including its timestamp.
   |  Version1 ObjectVersion
         -- ^ Just the version number.  For Commit, it is expected that
         -- ModifyUserInfo has already been used to set the information.
         -- For ModifyUserInfo, this option is illegal.
   |  Version1Plus ObjectVersion ObjectVersion
         -- ^ Just the version number and the head parent version to use
         -- (in that order)
   deriving (Show)

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
         changed :: ChangeData
         }
         -- Location exists in one of the parent versions, namely existsIn, 
         -- but has been changed.
   |  IsNew {changed :: ChangeData}
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
   deriving (Show)

type ChangeData = Either ICStringLen (Location,ObjectVersion)
   -- This indicates the contents of a changed item.  
   -- If (Left ...) this is raw data.
   -- If (Right ...) this means this item is in fact exactly the
   --    same as the one in (Location,ObjectVersion), a situation which
   --    arises, for example, during merging.

-- -----------------------------------------------------------------------
-- Instances of HasBinary
-- -----------------------------------------------------------------------

instance Monad m => HasBinary Location m where
   writeBin = mapWrite (\ (Location i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (Location i))

instance Monad m => HasBinary PrimitiveLocation m where
   writeBin = mapWrite (\ (PrimitiveLocation i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (PrimitiveLocation i))

instance MonadIO m => HasWrapper SimpleDBCommand m where
   wraps = [
      wrap0 0 NewLocation,
      wrap0 1 NewVersion,
      wrap0 2 ListVersions,
      wrap2 3 Retrieve,
      wrap2 4 LastChange,
      wrap3 5 Commit,
      wrap1 6 ModifyUserInfo,
      wrap1 7 GetVersionInfo,
      wrap2 8 GetDiffs,
      wrap1 9 MultiCommand
      ]
   unWrap = (\ wrapper -> case wrapper of
      NewLocation -> UnWrap 0 ()
      NewVersion -> UnWrap 1 ()
      ListVersions -> UnWrap 2 ()
      Retrieve l v -> UnWrap 3 (l,v)
      LastChange l v -> UnWrap 4 (l,v)
      Commit v r n -> UnWrap 5 (v,r,n)
      ModifyUserInfo v -> UnWrap 6 v
      GetVersionInfo v -> UnWrap 7 v
      GetDiffs v vs -> UnWrap 8 (v,vs)
      MultiCommand l -> UnWrap 9 l
      )

instance (MonadIO m,HasWrapper SimpleDBCommand m) 
      => HasBinary SimpleDBCommand m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped


instance MonadIO m => HasWrapper VersionInformation m where
   wraps = [
      wrap1 0 UserInfo1,
      wrap1 1 VersionInfo1,
      wrap1 2 Version1,
      wrap2 3 Version1Plus
      ]
   unWrap = (\ wrapper -> case wrapper of
      UserInfo1 u -> UnWrap 0 u
      VersionInfo1 v -> UnWrap 1 v
      Version1 v -> UnWrap 2 v
      Version1Plus v p -> UnWrap 3 (v,p)
      )

instance (MonadIO m,HasWrapper VersionInformation m) 
      => HasBinary VersionInformation m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped

instance MonadIO m => HasWrapper SimpleDBResponse m where
   wraps = [
      wrap1 0 IsLocation,
      wrap1 1 IsObjectVersion,
      wrap1 2 IsObjectVersions,
      wrap1 3 IsData,
      wrap1 4 IsError,
      wrap1 5 IsDiffs,
      wrap1 6 IsVersionInfo,
      wrap0 7 IsOK,
      wrap1 8 MultiResponse,
      wrap1 9 IsNotFound
      ]
   unWrap = (\ wrapper -> case wrapper of
      IsLocation l -> UnWrap 0 l
      IsObjectVersion v -> UnWrap 1 v
      IsObjectVersions vs -> UnWrap 2 vs
      IsData d -> UnWrap 3 d
      IsError e -> UnWrap 4 e
      IsDiffs ds -> UnWrap 5 ds
      IsVersionInfo v -> UnWrap 6 v
      IsOK -> UnWrap 7 ()
      MultiResponse l -> UnWrap 8 l
      IsNotFound s -> UnWrap 9 s
      )

instance (MonadIO m,HasWrapper SimpleDBResponse m) 
      => HasBinary SimpleDBResponse m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped


instance MonadIO m => HasWrapper Diff m where
   wraps = [
      wrap0 1 IsOld,
      wrap2 2 IsChanged,
      wrap1 3 IsNew
      ]

   unWrap = (\ wrapper -> case wrapper of
      IsOld -> UnWrap 1 ()
      IsChanged e c -> UnWrap 2 (e,c)
      IsNew c -> UnWrap 3 c
      )

instance (MonadIO m,HasWrapper Diff m) 
      => HasBinary Diff m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped


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
newtype PrimitiveLocation = PrimitiveLocation Int deriving (Eq,Ord)



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
   deriving (Show) -- allows us to dump the objectLocs file

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
            icslOptWE <- retrieveData simpleDB location objectVersion
            case fromWithError icslOptWE of
               Left mess -> return (IsNotFound mess)
               Right icsl -> return (IsData icsl)
      Commit versionInformation redirects newStuff0 ->
         let
            unpickVersionInformation :: IO SimpleDBResponse
            unpickVersionInformation = case versionInformation of
               UserInfo1 userInfo ->
                  do
                     versionInfoWE <- mkVersionInfo versionState user
                        (Left userInfo)
                     commitVersionInfoWE versionInfoWE
               VersionInfo1 versionInfo ->
                  do
                     objectVersionOpt <- lookupServerInfo versionState
                        (server versionInfo)
                     case objectVersionOpt of
                        Just objectVersion -> alreadyExists objectVersion
                        Nothing ->
                           do
                              versionInfoWE <- mkVersionInfo versionState user
                                (Right versionInfo)
                              commitVersionInfoWE versionInfoWE
               Version1 version ->
                  do
                     versionInfoOpt <- lookupVersionInfo versionState
                        version
                     case versionInfoOpt of
                        Just versionInfo -> commit Nothing versionInfo
                        Nothing -> toError ("Attempt to commit to " ++
                           toString version ++ " although no VersionInfo"
                           ++ " is known for this version.")
               Version1Plus version headVersion ->
                  do
                     versionInfoOpt <- lookupVersionInfo versionState
                        version
                     case versionInfoOpt of
                        Just versionInfo 
                           -> commit (Just headVersion) versionInfo
                        Nothing -> toError ("Attempt to commit to " ++
                           toString version ++ " although no VersionInfo"
                           ++ " is known for this version.")

            alreadyExists :: ObjectVersion -> IO SimpleDBResponse
            alreadyExists objectVersion =
               return (IsObjectVersion objectVersion)

            commitVersionInfoWE 
               :: WithError VersionInfo -> IO SimpleDBResponse
            commitVersionInfoWE versionInfoWE =
               case fromWithError versionInfoWE of
                  Left mess -> toError mess
                  Right versionInfo -> commit Nothing versionInfo 

            toError :: String -> IO SimpleDBResponse
            toError mess = return (IsError mess)

            commit :: Maybe ObjectVersion -> VersionInfo -> IO SimpleDBResponse
            commit headVersionOpt versionInfo0 =
               case commitVersionInfo versionInfo0 of
                  Nothing -> alreadyExists (version 
                     (VersionInfo.user versionInfo0))
                  Just versionInfo1 -> commit1 headVersionOpt versionInfo1

            commit1 
               :: Maybe ObjectVersion -> VersionInfo -> IO SimpleDBResponse
            commit1 headVersionOpt versionInfo =
               do
                  let
                     thisVersion = version (VersionInfo.user versionInfo)

                     parentVersionOpt = 
                        case (headVersionOpt,
                              parents (VersionInfo.user versionInfo)) of
                           (Just parentVersion,_) -> headVersionOpt
                           (Nothing,[]) -> Nothing
                           (Nothing,head : _) -> Just head

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

                           -- transmit extra version data if necessary
                           -- (We do this even for Version1, since the
                           -- isPresent flag will have been set.)
                           addVersionInfo versionState versionInfo
                           return IsOK
                     Left mess -> 
                        do
                           abortTransaction txn
                           return (IsError mess)
         in
            unpickVersionInformation
      ModifyUserInfo versionInformation ->  
         do
            let
               objectVersion = versionInformationToVersion versionInformation

            -- check whether the version is already known with a different
            -- version
            oldVersionOpt <- case versionInformation of
               VersionInfo1 versionInfo -> lookupServerInfo versionState 
                  (server versionInfo)
               _ -> return Nothing

            case oldVersionOpt of
               Just oldVersion | oldVersion /= objectVersion ->
                  return (IsObjectVersion oldVersion)
               Nothing ->
                  do
                     oldVersionInfoOpt 
                        <- lookupVersionInfo versionState objectVersion
                     let
                        versionErrorMess = "ModifyUserInfo: given Version1 "
                           ++ "argument, which is illegal"
                        versionError = Left versionErrorMess

                     case oldVersionInfoOpt of
                        Nothing -> 
                           do
                              versionInfoOrError <- case versionInformation of
                                 UserInfo1 userInfo ->
                                    do
                                       versionInfoWE <- mkVersionInfo 
                                          versionState user (Left userInfo)
                                       return (fromWithError versionInfoWE)
                                 VersionInfo1 versionInfo ->
                                    do
                                       versionInfoWE <- mkVersionInfo 
                                          versionState user (Right versionInfo)
                                       return (fromWithError versionInfoWE)
                                 Version1 version -> return versionError
                              case versionInfoOrError of
                                 Right versionInfo1 -> 
                                    do
                                       addVersionInfo versionState versionInfo1
                                       return IsOK
                                 Left mess -> return (IsError mess)
                        Just versionInfo0 ->
                           do
                              let
                                 versionInfo1WE = case versionInformation of
                                    UserInfo1 userInfo -> 
                                       changeUserInfo user versionInfo0 
                                          userInfo
                                    VersionInfo1 versionInfo -> 
                                       changeVersionInfo user versionInfo0 
                                          versionInfo
                                    Version1 version -> 
                                       hasError versionErrorMess
                              case fromWithError versionInfo1WE of
                                 Right versionInfo1 ->
                                    do
                                       addVersionInfo versionState versionInfo1
                                       return IsOK
                                 Left mess -> return (IsError mess)
      GetVersionInfo objectVersion ->
         do
            versionInfoOpt <- lookupVersionInfo versionState objectVersion
            return (case versionInfoOpt of
               Nothing -> IsNotFound "Version info"
               Just versionInfo -> IsVersionInfo versionInfo
               )
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

retrieveData :: SimpleDB -> Location -> ObjectVersion 
   -> IO (WithError ICStringLen)
retrieveData simpledb location objectVersion =
   do
      bdbKeyOptWE <- retrieveKeyOpt simpledb location objectVersion
      mapWithErrorIO' (
         \ bdbKeyOpt -> case bdbKeyOpt of
            Nothing -> return (hasError ("BDB key for " 
               ++ toString objectVersion ++ ":" ++ show location 
               ++ " mysteriously not in database"))
            Just bdbKey ->
               do
                  Just icsl <- readBDB (bdb simpledb) bdbKey
                  return (hasValue icsl)
            )
         bdbKeyOptWE

retrieveKeyOpt :: SimpleDB -> Location -> ObjectVersion 
   -> IO (WithError (Maybe BDBKey))
retrieveKeyOpt simpleDB location objectVersion =
   do
      versionMap <- readIORef (versionDictionary simpleDB)
      return (case lookupFM versionMap objectVersion of
         Nothing -> 
            hasError versionNotFoundMess
         Just versionData ->
            hasValue (retrieveKey1 versionData location)
         )

versionNotFoundMess :: String
versionNotFoundMess = "Retrieve failed; version does not exist!"

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

instance Monad m => HasWrapper ServerOp m where
   wraps = [
      wrap4 0 FrozenVersion,
      wrap0 1 AllocVersion,
      wrap0 2 AllocLocation
      ]

   unWrap = (\ wrapper -> case wrapper of
      FrozenVersion p t o r -> UnWrap 0 (p,t,o,r)
      AllocVersion -> UnWrap 1 ()
      AllocLocation -> UnWrap 2 ()
      )

instance (MonadIO m,HasWrapper ServerOp m) 
      => HasBinary ServerOp m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped



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
                                 return (
                                    if locationsSame location primLoc
                                       then
                                          redirects0
                                       else
                                          addToFM redirects0 location primLoc
                                    )
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
      nextLocation <- newIORef 2
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
      case vDatas of
         [] -> -- we just have to return IsNew for everything
            mapM
               (\ location ->
                  do
                     icslWE <- retrieveData db location version0
                     icsl <- coerceWithErrorOrBreakIO break icslWE
                     return (location,IsNew {changed = Left icsl})
                  )
               (getLocations vData0)
         (headVData :_) ->
            do
               let
                  -- Construct a map back from BDBKey 
                  -- -> (Location,ObjectVersion) for the parents.
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
                              (\ map0 location 
                                 -> addToFM map0 location version)
                              map0
                              (getLocations vData)
                        )
                     emptyFM
                     vDatas 

                  -- Function constructing Diff for a particular item in the 
                  -- new version's object dictionary
                  mkDiff :: Location -> BDBKey -> IO Diff
                  mkDiff location key1 =
                     case retrieveKey1 headVData location of
                        Just key2 | key1 == key2 
                           -> return IsOld
                        _ -> 
                           do
                              changed <- case lookupFM bdbDict key1 of
                                 Just locVers -> return (Right locVers)
                                 Nothing -> 
                                    do
                                       (Just icsl) <- readBDB (bdb db) key1
                                       return (Left icsl)
                              return (case lookupFM locationMap location of
                                 Nothing -> IsNew {changed = changed}
                                 Just version ->
                                    IsChanged {existsIn = version,
                                       changed = changed}
                                 )
               mapM
                  (\ location ->
                     do
                        let
                           Just key = retrieveKey1 vData0 location
                        diff <- mkDiff location key
                        return (location,diff)
                     )
                  (getLocations vData0)
   )

-- -------------------------------------------------------------------
-- VersionInformation operations
-- -------------------------------------------------------------------

versionInformationToVersion :: VersionInformation -> ObjectVersion
versionInformationToVersion versionInformation = case versionInformation of
   UserInfo1 userInfo -> version userInfo
   VersionInfo1 versionInfo -> version (user versionInfo)
   Version1 objectVersion -> objectVersion

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

locationsSame :: Location -> PrimitiveLocation -> Bool
locationsSame (Location loc1) (PrimitiveLocation loc2) = loc1 == loc2
      
retrievePrimitiveLocation :: VersionData -> Location -> PrimitiveLocation
retrievePrimitiveLocation versionData =
   retrievePrimitiveLocation1 (redirects versionData)

retrievePrimitiveLocation1 :: FiniteMap Location PrimitiveLocation 
    -> Location -> PrimitiveLocation
retrievePrimitiveLocation1 map (location @ (Location locNo)) =
   lookupWithDefaultFM map (PrimitiveLocation locNo) location



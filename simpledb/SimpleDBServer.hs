{- This module introduces the functions available to the server to access
   the database.  

   NB.  At various points we assume that these functions are only used
   in a single-threaded way.  The server code should ensure this. -}
module SimpleDBServer(
   SimpleDB, -- SimpleDB.  There should be only one of them per program.
   initialiseSimpleDB, -- :: IO SimpleDB
      -- Initialise database, reading backup off disk if necessary.
   backupSimpleDB, -- :: SimpleDB -> IO ()
      -- Save to disk.

   -- SimpleDBCommand/Response are the types of queries and responses
   -- to the DB.
   -- Each is an instance of Read and Show.
   SimpleDBCommand(..),
   SimpleDBResponse(..),

   -- Location of objects.  first/secondLocation point to the first
   -- and second ones NewLocation command ever allocates, and so are rather 
   -- special.
   Location,
   firstLocation, -- :: Location
   secondLocation, -- :: Location
   -- Likewise firstVersion points to the first version allocated for
   -- any object.
   firstVersion, -- :: Version

   -- Type of object versions.
   ObjectVersion,

   querySimpleDB, -- :: SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
   ) where

import IO
import Maybe

import FiniteMap

import AtomString
import IOExts
import Dynamics

import CacheTable
import IOExtras
import WBFiles

import BDBClient

-- -------------------------------------------------------------------
-- The query types.
-- -------------------------------------------------------------------

newtype Location = Location Int

firstLocation :: Location
-- The magic number 0 is also used by initialiseSimpleDB
firstLocation = Location 0

secondLocation :: Location
secondLocation = Location 1

newtype ObjectVersion = ObjectVersion Int

firstVersion :: ObjectVersion
firstVersion = ObjectVersion 0

data SimpleDBCommand =
      NewLocation -- returns with IsLocation
   |  Commit BDBKey Location (Maybe ObjectVersion)
          -- commits a new version to the repository.  To ease storage,
          -- the parent version is supplied UNLESS this is the first time
          -- we commit to the location.  
          -- Responds with IsObjectVersion, giving the version of this file.
   |  Retrieve Location ObjectVersion
          -- Returns an existing version, which had better exist.
          -- Returns with IsContents 
   |  ListVersions Location
          -- Returns the versions known for this object.
          -- Returns with IsObjectVersions
      deriving (Read,Show)

data SimpleDBResponse =
      IsLocation Location
   |  IsObjectVersion ObjectVersion
   |  IsObjectVersions [ObjectVersion]
   |  IsContents BDBKey
      deriving (Read,Show)

-- -------------------------------------------------------------------
-- Instances for locations and objectVersions
-- -------------------------------------------------------------------

instance StringClass Location where
   toString (Location i) = show i
   fromString s = Location (read s)

instance Eq Location where
   (==) (Location l1) (Location l2) = (==) l1 l2
   (/=) (Location l1) (Location l2) = (/=) l1 l2

instance Ord Location where
   compare (Location l1) (Location l2) = compare l1 l2

location_tyRep = mkTyRep "SimpleDBServer" "Location"
instance HasTyRep Location where
   tyRep _ = location_tyRep

instance StringClass ObjectVersion where
   toString (ObjectVersion i) = show i
   fromString s = ObjectVersion (read s)

instance Eq ObjectVersion where
   (==) (ObjectVersion l1) (ObjectVersion l2) = (==) l1 l2
   (/=) (ObjectVersion l1) (ObjectVersion l2) = (/=) l1 l2

instance Ord ObjectVersion where
   compare (ObjectVersion l1) (ObjectVersion l2) = compare l1 l2

objectVersion_tyRep = mkTyRep "SimpleDBServer" "ObjectVersion"
instance HasTyRep ObjectVersion where
   tyRep _ = objectVersion_tyRep

-- -------------------------------------------------------------------
-- The SimpleDB type.
-- -------------------------------------------------------------------

data SimpleDB = SimpleDB {
   -- objectLocations is a handle to a file to which we write
   -- the locations of all objects, as (Location,ObjectVersion,BDBKey).
   -- The pointer is always at the end of the file, after the initialisation
   -- is over.
   objectLocations :: Handle,
   -- Locations of all objects in the DB.
   objectDictionary 
      :: IORef (FiniteMap (Location,ObjectVersion) BDBKey),
   -- Next location to allocate
   nextLocation :: IORef Int,
   -- Next version for each allocated location to allocate.
   nextVersions :: IORef (FiniteMap Location Int)
   }

-- -------------------------------------------------------------------
-- The objectLocations file.
-- -------------------------------------------------------------------

-- Open and read it.
initObjectLocations :: IO (Handle,FiniteMap (Location,ObjectVersion) BDBKey)
initObjectLocations =
   do
      fpath <- getStoreDir
      handle <- openFile fpath ReadWriteMode
      let
         getRest map =
            do
               nextOpt <- readObjectLocations handle
               case nextOpt of
                  Nothing -> return map
                  Just (location,objectVersion,bdbKey) ->
                     getRest (addToFM map (location,objectVersion) bdbKey)
      map <- getRest emptyFM
      return (handle,map)                  

writeObjectLocations :: Handle -> Location -> ObjectVersion -> BDBKey
   -> IO ()
writeObjectLocations handle location objectVersion bdbKey =
   hPutStrLn handle (show (location,objectVersion,bdbKey))

-- readObjectLocations returns Nothing when at EOF
readObjectLocations :: Handle -> IO (Maybe (Location,ObjectVersion,BDBKey))
readObjectLocations handle = catchEOF (
   do
      line <- hGetLine handle
      return (read line)
   )

-- -------------------------------------------------------------------
-- Initialisation and backup
-- -------------------------------------------------------------------

initialiseSimpleDB :: IO SimpleDB
initialiseSimpleDB =
   do
      (objectLocations,objectDictionaryVal) <- initObjectLocations
      let
         -- Work out plausible value for nextLocation
         (nextLocationVal :: Int) =
            foldFM
               (\ (Location this,_) _ current ->
                  if this>=current then this+1 else current)
               2
               objectDictionaryVal
         -- Ditto for nextVersions, only it's more complicated.
         (nextVersionsVal :: FiniteMap Location Int) =
            foldFM
               (\ (location,ObjectVersion v) _ map ->
                  case lookupFM map location of
                     Nothing -> addToFM map location (v+1)
                     Just w -> 
                        if v>=w then addToFM map location (v+1) else map
                  )
               emptyFM
               objectDictionaryVal
      objectDictionary <- newIORef objectDictionaryVal
      nextLocation <- newIORef nextLocationVal
      nextVersions <- newIORef nextVersionsVal
      return (SimpleDB {
         objectLocations = objectLocations,
         objectDictionary = objectDictionary,
         nextLocation = nextLocation,
         nextVersions = nextVersions
         })

backupSimpleDB :: SimpleDB -> IO ()
backupSimpleDB simpleDB =
   do
      hFlush (objectLocations simpleDB)

querySimpleDB :: SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
querySimpleDB
      (SimpleDB {
         objectLocations = objectLocations,
         objectDictionary = objectDictionary,
         nextLocation = nextLocation,
         nextVersions = nextVersions
         })
      command =
   case command of
      NewLocation ->
         do
            loc <- readIORef nextLocation
            writeIORef nextLocation (loc + 1)
            return (IsLocation (Location loc))
      Commit bdbKey location _ ->
         do
            nextVersionsVal <- readIORef nextVersions
            -- get version
            let
               nextVersionInt = lookupWithDefaultFM nextVersionsVal 0 location
               nextVersion = ObjectVersion nextVersionInt
            writeIORef nextVersions 
               (addToFM nextVersionsVal location (nextVersionInt+1))
            -- add to dictionary
            objectDictionaryVal <- readIORef objectDictionary
            writeIORef objectDictionary
               (addToFM objectDictionaryVal (location,nextVersion) bdbKey)
            -- write to file
            writeObjectLocations objectLocations location nextVersion bdbKey
            -- return version
            return (IsObjectVersion nextVersion)   
      Retrieve location objectVersion ->
         do         
            objectDictionaryVal <- readIORef objectDictionary
            let
               bdbKey = lookupWithDefaultFM objectDictionaryVal 
                  (error "In retrieve, object not in dictionary")
                  (location,objectVersion)
            return (IsContents bdbKey)
      ListVersions location ->
         do
            -- This isn't very efficient, but it isn't used very often.
            objectDictionaryVal <- readIORef objectDictionary
            let
               versions = mapMaybe
                  (\ (location',version) ->
                     if location' == location
                        then 
                           Just version
                        else
                           Nothing
                     )  
                  (keysFM objectDictionaryVal)
            return (IsObjectVersions versions)
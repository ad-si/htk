{- This is the user-interface to the simpleDB.
   Any resemblance to the interface to the CVSDB module is entirely
   deliberate. 

   We implement all functions except getCVSFilePath and watch, which
   aren't used anyway, and also don't implement DirectAccess (ditto).
   -}
module SimpleDB(
   Repository,

   initialise, -- :: IO Repository

   ObjectVersion, 
   -- type of versions of objects in the repository
   -- instance of Read/Show/StringClass

   ObjectSource,
   -- type of data as retrieved from the repository.
   exportString, -- :: ObjectSource -> IO String
   -- exportString extracts the contents of the object as a String
   exportFile, -- :: ObjectSource -> FilePath -> IO ()
   -- exportFile writes the contents of the object as a file with the
   -- supplied name, overwriting whatever was there before.
   importString, -- :: String -> IO ObjectSource
   -- importString makes an object with the given contents.
   importFile, -- :: FilePath -> IO ObjectSource
   -- importFile makes an object from the given file.

   Location,
   -- represents Location of object in the repository.  Instance of 
   -- Read/Show/Eq.
   firstLocation, -- :: Location
   secondLocation, -- :: Location
   -- These two locations are special.  They are already allocated,
   -- and may be used as the user desires.

   firstVersion, -- :: ObjectVersion
   -- This should be the very first version the firstLocation object
   -- receives.

   newLocation, -- :: Repository -> IO Location
   -- allocate a new unique location in the repository.
   -- and the version of the associated attributes.

   commit, -- :: Repository -> ObjectSource -> Location 
      -- -> Maybe ObjectVersion -> IO ObjectVersion
   -- commits a new version of the object to the repository, returning its
   -- new version.  The version, if supplied, should be a previous version of
   -- this object.  If it is NOT supplied, this MUST be the first time
   -- we store to this object.


   -- Two-Stage commit   
   commitStage1,
      -- :: Repository -> Location -> Maybe ObjectVersion -> IO ObjectVersion
   commitStage2, 
      -- :: Repository -> ObjectSource -> Location -> ObjectVersion -> IO ()
   -- This divides the work of the commit function into two halves, the first
   -- of which allocates and returns a new object version, the second of 
   -- which actually commits.  commitStage2 should be given the ObjectVersion
   -- returned by commitStage1; of course this should also not be re-used.

   retrieveFile, -- :: Repository -> Location -> ObjectVersion -> FilePath ->
                 --       IO ()
   -- retrieveFile retrieves the given version of the object at Location
   -- by copying it to file at FilePath.
   retrieveString, -- :: Repository -> Location -> ObjectVersion -> IO String
   -- retrieveFile retrieves the given version of the object as a String

   listVersions, -- :: Repository -> Location -> IO [ObjectVersion]
   -- listVersion lists all versions of the object with the given location.

   ) where

import Object
import Computation(done)

import Destructible

import InfoBus

import CallServer

import CopyFile

import SimpleDBServer
import SimpleDBService
import BDBClient


----------------------------------------------------------------
-- The Repository type, its initialisation, and its destruction.
----------------------------------------------------------------

data Repository = Repository {
   bdb :: BDB,
   queryRepository :: SimpleDBCommand -> IO SimpleDBResponse,
   closeDown :: IO (),
   oID :: ObjectID
   }

initialise :: IO Repository
initialise =
   do
      (queryRepository,closeDown,"") <- connectReply simpleDBService
      bdb <- openBDB
      oID <- newObject
      let
         repository = Repository {
            bdb = bdb,
            queryRepository = queryRepository,
            closeDown = closeDown,
            oID = oID
            }

      registerTool repository
      return repository

instance Object Repository where
   objectID repository = oID repository

instance Destroyable Repository where
   destroy repository = closeDown repository
   
----------------------------------------------------------------
-- Query functions
----------------------------------------------------------------

newLocation :: Repository -> IO Location
newLocation repository =
   do
      response <- queryRepository repository NewLocation
      return (toLocation response)

commit :: Repository -> ObjectSource -> Location 
   -> Maybe ObjectVersion -> IO ObjectVersion
commit repository objectSource location parent =
   do
      bdbKey <- writeBDB (bdb repository) objectSource
      response <- queryRepository repository (Commit bdbKey location parent)
      return (toObjectVersion response)

commitStage1 
   :: Repository -> Location -> Maybe ObjectVersion -> IO ObjectVersion
commitStage1 repository location parent =
   do
      response <- queryRepository repository (CommitStage1 location parent)
      return (toObjectVersion response)

commitStage2 
   :: Repository -> ObjectSource -> Location -> ObjectVersion -> IO ()
commitStage2 repository objectSource location thisVersion =
   do
      bdbKey <- writeBDB (bdb repository) objectSource
      IsNothing <- queryRepository repository 
         (CommitStage2 bdbKey location thisVersion)
      done

retrieveObjectSource :: Repository -> Location -> ObjectVersion 
   -> IO ObjectSource
retrieveObjectSource repository location objectVersion =
   do
      response <- queryRepository repository (Retrieve location objectVersion)
      readBDB (bdb repository) (toContents response)

retrieveString :: Repository -> Location -> ObjectVersion -> IO String
retrieveString repository location objectVersion =
   do
      objectSource <- retrieveObjectSource repository location objectVersion
      exportString objectSource

retrieveFile :: Repository -> Location -> ObjectVersion -> FilePath -> IO ()
retrieveFile repository location objectVersion filePath =
   do
      objectSource <- retrieveObjectSource repository location objectVersion
      exportFile objectSource filePath

listVersions :: Repository -> Location -> IO [ObjectVersion]
listVersions repository location =
   do
      response <- queryRepository repository (ListVersions location)
      return (toObjectVersions response)

----------------------------------------------------------------
-- Unpacking SimpleDBResponse
----------------------------------------------------------------

toLocation :: SimpleDBResponse -> Location
toLocation (IsLocation location) = location
toLocation r = unpackError "location" r

toObjectVersion :: SimpleDBResponse -> ObjectVersion
toObjectVersion (IsObjectVersion objectVersion) = objectVersion
toObjectVersion r = unpackError "objectVersion" r

toObjectVersions :: SimpleDBResponse -> [ObjectVersion]
toObjectVersions (IsObjectVersions objectVersions) = objectVersions
toObjectVersions r = unpackError "objectVersions" r

toContents :: SimpleDBResponse -> BDBKey
toContents (IsContents bdbKey) = bdbKey
toContents r = unpackError "object" r

unpackError s r = error ("Expecting "++s++" in "++show r)


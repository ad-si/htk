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

import Destructible

import InfoBus

import CallServer

import CopyFile

import SimpleDBServer
import SimpleDBService


----------------------------------------------------------------
-- The Repository type, its initialisation, and its destruction.
----------------------------------------------------------------

data Repository = Repository {
   queryRepository :: SimpleDBCommand -> IO SimpleDBResponse,
   closeDown :: IO (),
   oID :: ObjectID
   }

initialise :: IO Repository
initialise =
   do
      (queryRepository,closeDown,"") <- connectReply simpleDBService
      oID <- newObject
      let
         repository = Repository {
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
-- The ObjectSource type, and functions for it.
----------------------------------------------------------------

data ObjectSource = 
      FileObject String
   |  StringObject String

exportString :: ObjectSource -> IO String
exportString (StringObject str) = return str
exportString (FileObject name) = copyFileToString name

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile (FileObject source) destination = copyFile source destination
exportFile (StringObject str) destination = copyStringToFile str destination

importString :: String -> IO ObjectSource
importString str = return (StringObject str)

importFile :: FilePath -> IO ObjectSource
importFile file = return (FileObject file)


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
      str <- exportString objectSource
      response <- queryRepository repository (Commit str location parent)
      return (toObjectVersion response)

retrieveString :: Repository -> Location -> ObjectVersion -> IO String
retrieveString repository location objectVersion =
   do
      response <- queryRepository repository (Retrieve location objectVersion)
      return (toContents response)

retrieveFile :: Repository -> Location -> ObjectVersion -> FilePath -> IO ()
retrieveFile repository location objectVersion filePath =
   do
      string <- retrieveString repository location objectVersion
      -- retrieveFile retrieves the given version of the object at Location
      -- by copying it to file at FilePath.
      copyStringToFile string filePath

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

toContents :: SimpleDBResponse -> String
toContents (IsContents contents) = contents
toContents r = unpackError "object" r

unpackError s r = error ("Expecting "++s++" in "++show r)


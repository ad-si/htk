{- This is the user-interface to the SimpleDB.
   -}
module SimpleDB(
   Repository,
      -- represents a connection to a particular server.
      -- Instance of Ord/Eq.

   initialise, -- :: (?server :: HostPort) => IO Repository
   toServer, -- :: Repository -> HostPort
      -- extract the HostPort with which this Repository was created.

   ObjectVersion, 
   -- type of versions of objects in the repository
   -- instance of Read/Show/StringClass

   module ObjectSource,

   Location,
   -- represents Location of object in the repository.  Instance of 
   -- Read/Show/Eq.
   specialLocation1, -- :: Location
   specialLocation2, -- :: Location
   -- These two locations are special.  They are already allocated,
   -- and may be used as the user desires.

   firstVersion, -- :: ObjectVersion
   -- This should be the very first version the specialLocation1 object
   -- receives.

   newLocation, -- :: Repository -> IO Location
   -- allocate a new unique location in the repository.

   newVersion, -- :: Repository -> IO Location
   -- allocate a new unique version in the repository.

   lastChange, 
      -- :: Repository -> Location -> ObjectVersion -> IO ObjectVersion
      -- return the ObjectVersion in which this object was last updated.
   retrieveFile, -- :: Repository -> Location -> ObjectVersion -> FilePath ->
                 --       IO ()
   -- retrieveFile retrieves the given version of the object at Location
   -- by copying it to file at FilePath.
   retrieveString, -- :: Repository -> Location -> ObjectVersion -> IO String
   -- retrieveFile retrieves the given version of the object as a String

   listVersions, -- :: Repository -> IO [ObjectVersion]
   -- listVersion lists all versions in the repository.

   commit,
      --  :: Repository 
      --  -> Either UserInfo VersionInfo
      --  -> [(Location,Maybe ObjectVersion)]
      --  -> [(Location,CommitChange)] -> IO ()
      -- Commit a complete new version to the repository.
      --
      -- Either UserInfo VersionInfo
      --    contains the additional information for this commit
      --    (the ObjectVersion for the new version, parent versions,
      --       version title, and so on.)
      -- [(Location,Maybe ObjectVersion)]
      --    redirects.  I can't be bothered to explain them now, see
      --    definition of SimpleDBServer.Commit.  For normal (non-session
      --    management) commits, this list will be empty.
      -- [(Location,Either ObjectSource (Location,ObjectVersion))] 
      --    is the list of updates.  Later updates take priority over
      --    earlier ones. 
      --    An update is either Left ObjectSource, for completely new text,
      --    or Right (loc,version), indicating the same text of an existing
      --    version.  
      --       NB.  The locations list ONLY has to contain what's different
      --       from the parent version, if any.  The Right .. option is
      --       normally only used during merging.

   CommitChange, -- = Either ObjectSource (Location,ObjectVersion)

   modifyUserInfo,
      -- :: Repository -> UserInfo -> IO ()

   getDiffs,
      -- :: Repository -> ObjectVersion -> [ObjectVersion] 
      -- -> IO [(Location,Diff)]
      -- Compare the given object version with the (presumably parent)
      -- object versions.
   Diff(..),
   ) where

import Object
import Computation(done)
import BinaryIO
import ICStringLen
import Debug(debug)
import ExtendedPrelude

import Destructible

import InfoBus

import HostsPorts
import CallServer
import MultiPlexer

import CopyFile

import SimpleDBServer
import SimpleDBService
import VersionInfo hiding (server)
import ObjectSource hiding (getICSL,fromICSL)
   -- that prevents those two functions being exported
import qualified ObjectSource


----------------------------------------------------------------
-- The Repository type, its initialisation, and its destruction.
----------------------------------------------------------------

data Repository = Repository {
   queryRepository :: SimpleDBCommand -> IO SimpleDBResponse,
   closeDown :: IO (),
   oID :: ObjectID,
   server :: HostPort
   }

initialise :: (?server :: HostPort) => IO Repository
initialise =
   do
      (queryRepository0,closeDown,"") <- connectReply simpleDBService

      let
         queryRepository1 :: SimpleDBCommand -> IO SimpleDBResponse
         queryRepository1 =
#ifdef DEBUG
            \ simpleDBCommand -> do
               debug simpleDBCommand
               response <- queryRepository0 simpleDBCommand
               debug response
               return response
#else
            queryRepository0
#endif

         queryRepository2 :: [SimpleDBCommand] -> IO [SimpleDBResponse]
         queryRepository2 [] = return []
         queryRepository2 [command] =
            do
               response <- queryRepository1 command
               return [response]
         queryRepository2 commands =         
            do
               (MultiResponse responses) <- queryRepository1 
                  (MultiCommand commands)
               return responses

      multiPlexer <- newMultiPlexer queryRepository2

      let
         queryRepository3 :: SimpleDBCommand -> IO SimpleDBResponse
         queryRepository3 command =
            do
               response <- sendCommand multiPlexer command
               case response of
                  IsError mess -> error mess
                  _ -> return response


      oID <- newObject
      let
         repository = Repository {
            queryRepository = queryRepository3,
            closeDown = closeDown,
            oID = oID,
            server = ?server
            }

      registerTool repository
      return repository

toServer :: Repository -> HostPort
toServer = server

instance Object Repository where
   objectID repository = oID repository

instance Destroyable Repository where
   destroy repository = closeDown repository

instance Eq Repository where
   (==) = mapEq oID

instance Ord Repository where
   compare = mapOrd oID

----------------------------------------------------------------
-- Query functions
----------------------------------------------------------------

newLocation :: Repository -> IO Location
newLocation repository =
   do
      response <- queryRepository repository (NewLocation)
      return (toLocation response)

newVersion :: Repository -> IO ObjectVersion
newVersion repository =
   do
      response <- queryRepository repository (NewVersion)
      return (toObjectVersion response)

lastChange :: Repository -> Location -> ObjectVersion -> IO ObjectVersion
lastChange repository location objectVersion =
   do
      response 
         <- queryRepository repository (LastChange location objectVersion)
      return (toObjectVersion response)

listVersions :: Repository -> IO [ObjectVersion]
listVersions repository =
   do
      response <- queryRepository repository ListVersions 
      return (toObjectVersions response)

retrieveObjectSource :: Repository -> Location -> ObjectVersion 
   -> IO ObjectSource
retrieveObjectSource repository location objectVersion =
   do
      response <- queryRepository repository (Retrieve location objectVersion)
      let
         icsl = toData response
      seq icsl done
      return (ObjectSource.fromICSL icsl)

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

type CommitChange = Either ObjectSource (Location,ObjectVersion)

commit :: Repository 
   -> Either UserInfo VersionInfo
   -> [(Location,Maybe ObjectVersion)]
   -> [(Location,CommitChange)] -> IO ()
commit repository versionExtra redirects newStuff0 =
   do
      (newStuff1 
            :: [(Location,Either ICStringLen (Location,ObjectVersion))]) <-
         mapM
            (\ (location,newItem) ->
               case newItem of
                  Left objectSource ->
                     do
                        icsl <- ObjectSource.getICSL objectSource
                        return (location,Left icsl)
                  Right locVers -> return (location,Right locVers)
               )
            newStuff0

      response <- queryRepository repository (
         Commit versionExtra redirects newStuff1)

      case response of
         IsOK -> done
         _ -> error ("Commit error: unexpected response")


modifyUserInfo repository userInfo =
   do
      response <- queryRepository repository (ModifyUserInfo userInfo)
      case response of
         IsOK -> done
         _ -> error ("ModifyUserInfo: unexpected response")


getDiffs :: Repository -> ObjectVersion -> [ObjectVersion] 
   -> IO [(Location,Diff)]
getDiffs repository version versions =
   do
      response <- queryRepository repository (GetDiffs version versions)
      case response of
         IsDiffs diffs -> return diffs
         _ -> error ("GetDiffs: unexpected response")

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

toData :: SimpleDBResponse -> ICStringLen
toData (IsData icsl) = icsl
toData r = unpackError "object" r

unpackError s r = error ("Expecting " ++ s ++ ": " ++ 
   case r of
      IsError mess -> mess
      _ -> " but found something else"
   ) 


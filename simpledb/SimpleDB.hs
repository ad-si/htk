-- |
-- Description: Low-level Repository Functions
-- 
-- This is the user-interface to the SimpleDB.
module SimpleDB(
   Repository,
      -- represents a connection to a particular server.
      -- Instance of Ord/Eq.

   initialise, -- :: (?server :: HostPort) => IO Repository
   initialiseInternal, -- :: VersionState -> IO Repository

   ObjectVersion, 
   -- type of versions of objects in the repository
   -- instance of Show/StringClass

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

   newVersion, -- :: Repository -> IO ObjectVersion
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
   retrieveObjectSource, 
      -- :: Repository -> Location -> ObjectVersion -> IO ObjectSource
      -- retrieves the given version as an ObjectSource.

   listVersions, -- :: Repository -> IO [ObjectVersion]
   -- listVersion lists all versions in the repository.

   commit,
      --  :: Repository 
      --  -> VersionInformation
      --  -> [(Location,Either ObjectVersion (Maybe Location))]
      --  -> [(Location,CommitChange)] -> IO ()
      -- Commit a complete new version to the repository.
      --
      -- VersionInformation
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
      -- [(Location,Location)]
      --    Changes to parents.  Again, see SimpleDBServer.Commit.

   CommitChange, -- = Either ObjectSource (Location,ObjectVersion)
   VersionInformation(..),

   modifyUserInfo,
      -- :: Repository -> UserInfo -> IO ()
   modifyVersionInfo,
      -- :: Repository -> VersionInfo -> IO ()

   getDiffs,
      -- :: Repository -> ObjectVersion -> [ObjectVersion] 
      -- -> IO ([(Location,Diff)],[(Location,Location)])
      -- Compare the given object version with the (presumably parent)
      -- object versions.

   Permissions,Permission(..),
   getPermissions,
      -- :: Repository -> Maybe (ObjectVersion,Location) -> IO Permissions
      -- get the permissions for the given location, or if none is given
      -- the global permissions.

   setPermissions,
      -- :: Repository -> Maybe (ObjectVersion,Location) -> Permissions
      -- -> IO ()
      -- set permissions.

   getParentLocation,
      -- :: Repository -> (ObjectVersion,Location) -> IO (Maybe Location)
      -- Get the object's parent location.
   setAdminStatus, 
      -- :: Repository -> Bool -> IO ()

   Diff(..),
   ChangeData,

   queryRepository,
      -- :: Repository -> SimpleDBCommand -> IO SimpleDBResponse,
      -- General query.
   SimpleDBCommand(..),SimpleDBResponse(..),

   module ServerErrors,
 
   catchNotFound,
      -- :: IO a -> IO (Maybe a)
      -- Detect a NotFound error and replacing it by 'Nothing'.

   ) where

import Control.Concurrent.MVar

import Object
import Computation(done)
import ICStringLen
import Debug(debug)
import ExtendedPrelude
import AtomString
import CompileFlags

import Destructible
import BSem
import Synchronized

import InfoBus

import HostsPorts
import HostsList(defaultUser)
import CallServer
import MultiPlexer
import PasswordFile

import SimpleDBServer
import SimpleDBService
import VersionInfo hiding (server)
import VersionState
import ObjectSource
   -- that prevents those two functions being exported
import qualified ObjectSource
import Permissions
import ServerErrors


----------------------------------------------------------------
-- The Repository type, its initialisation, and its destruction.
----------------------------------------------------------------

data Repository = Repository {
   queryRepository :: SimpleDBCommand -> IO SimpleDBResponse,
   closeDown :: IO (),
   oID :: ObjectID
   }

initialise :: (?server :: HostPort) => IO Repository
initialise =
   do
      (queryRepository0,closeDown,"") <- connectReply simpleDBService

      let
         queryRepository1 :: SimpleDBCommand -> IO SimpleDBResponse
         queryRepository1 =
           if isDebug
              then
                 (\ simpleDBCommand -> 
                    do
                       debug simpleDBCommand
                       response <- queryRepository0 simpleDBCommand
                       debug response
                       return response
                    )
              else
                 queryRepository0

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

      initialise1 (sendCommand multiPlexer) closeDown

initialiseInternal :: VersionState -> IO Repository
initialiseInternal versionState =
   do
      simpleDB <- openSimpleDB versionState

      bSem <- newBSem

   

      adminMVar <- newMVar False

      seq defaultUser done
      let
         userId1 = defaultUser

      user <- createUser userId1

      let
         queryRepository command = 
            synchronize bSem (querySimpleDB user simpleDB command)

      initialise1 queryRepository done

initialise1 :: (SimpleDBCommand -> IO SimpleDBResponse) -> IO () 
   -> IO Repository
initialise1 queryRepository1 closeDown =
   do
      let
         queryRepository2 :: SimpleDBCommand -> IO SimpleDBResponse
         queryRepository2 command =
            do
               response <- queryRepository1 command
               case response of
                  IsError errorType mess -> throwError errorType mess
                  _ -> return response

      oID <- newObject
      let
         repository = Repository {
            queryRepository = queryRepository2,
            closeDown = closeDown,
            oID = oID
            }

      registerTool repository
      return repository

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
      response <- queryRepository repository NewLocation
      return (toLocation response)

newVersion :: Repository -> IO ObjectVersion
newVersion repository =
   do
      response <- queryRepository repository (NewVersion)
      return (toObjectVersion response)

lastChange :: Repository -> ObjectVersion -> Location -> IO ObjectVersion
lastChange repository objectVersion location =
   do
      response 
         <- queryRepository repository (LastChange objectVersion location)
      return (toObjectVersion response)

listVersions :: Repository -> IO [ObjectVersion]
listVersions repository =
   do
      response <- queryRepository repository ListVersions 
      return (toObjectVersions response)

retrieveObjectSource :: Repository -> ObjectVersion -> Location 
   -> IO ObjectSource
retrieveObjectSource repository objectVersion location =
   do
      response <- queryRepository repository (Retrieve objectVersion location)
      let
         icsl = toData response
      seq icsl done

      importICStringLen icsl

retrieveString :: Repository -> ObjectVersion -> Location -> IO String
retrieveString repository objectVersion location =
   do
      objectSource <- retrieveObjectSource repository objectVersion location
      exportString objectSource

retrieveFile :: Repository -> ObjectVersion -> Location -> FilePath -> IO ()
retrieveFile repository objectVersion location filePath =
   do
      objectSource <- retrieveObjectSource repository objectVersion location
      exportFile objectSource filePath

type CommitChange = Either ObjectSource (ObjectVersion,Location)

commit :: Repository 
   -> VersionInformation
   -> [(Location,Maybe ObjectVersion)]
   -> [(Location,CommitChange)] 
   -> [(Location,Location)]
   -> IO ()
commit repository versionInformation redirects newStuff0 parentChanges =
   do
      (newStuff1 
            :: [(Location,Either ICStringLen (ObjectVersion,Location))]) <-
         mapM
            (\ (location,newItem) ->
               case newItem of
                  Left objectSource ->
                     do
                        icsl <- ObjectSource.exportICStringLen objectSource
                        return (location,Left icsl)
                  Right locVers -> return (location,Right locVers)
               )
            newStuff0

      response <- queryRepository repository (
         Commit versionInformation redirects newStuff1 parentChanges)

      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> 
            throwError MiscError ("ObjectVersion " ++ show objectVersion
               ++ " cannot be committed as it already exists")
         _ -> unpackError "commit" response



modifyUserInfo :: Repository -> UserInfo -> IO ()
modifyUserInfo repository userInfo =
   do
      response 
         <- queryRepository repository (ModifyUserInfo (UserInfo1 userInfo))
      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> 
            error ("modifyUserInfo: " ++ toString objectVersion)
         _ -> unpackError "modifyUserInfo" response

modifyVersionInfo :: Repository -> VersionInfo -> IO ()
modifyVersionInfo repository versionInfo =
   do
      response 
         <- queryRepository repository 
            (ModifyUserInfo (VersionInfo1 versionInfo))
      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> 
            throwError MiscError ("ObjectVersion " ++ show objectVersion
               ++ " cannot be modified as it already exists")
         _ -> unpackError "modifyVersionInfo" response

getDiffs :: Repository -> ObjectVersion -> [ObjectVersion] 
   -> IO ([(Location,Diff)],[(Location,Location)])
getDiffs repository version versions =
   do
      response <- queryRepository repository (GetDiffs version versions)
      case response of
         IsDiffs diffs1 diffs2 -> return (diffs1,diffs2)
         _ -> unpackError "getDiffs" response

----------------------------------------------------------------
-- Permissions operations.
----------------------------------------------------------------

-- | Retrieve the permissions for the object with given object version
-- and location.
getPermissions 
   :: Repository -> Maybe (ObjectVersion,Location) -> IO Permissions
getPermissions repository ovOpt =
   do
      response <- queryRepository repository (GetPermissions ovOpt)
      case response of
         IsPermissions permissions -> return permissions
         _ -> unpackError "getPermissions" response

-- | Set the permissions for the object with given object version and
-- location.
setPermissions 
   :: Repository -> Maybe (ObjectVersion,Location) -> Permissions -> IO ()
setPermissions repository ovOpt permissions =
   do
      response <- queryRepository repository (SetPermissions ovOpt permissions)
      case response of
         IsOK -> done
         _ -> unpackError "setPermissions" response

-- | Get the parent location of the object with given object version and
-- location.
getParentLocation 
   :: Repository -> ObjectVersion -> Location -> IO (Maybe Location)
getParentLocation repository version location =
   do
      response <- queryRepository repository 
         (GetParentLocation (version,location))
      case response of
         IsLocation location -> return (Just location)
         IsOK -> return Nothing
         _ -> unpackError "getParentLocation" response

-- | Claim admin status (if the bool is 'True') or revoke it if 'False'.
setAdminStatus :: Repository -> Bool -> IO ()
setAdminStatus repository isClaim =
   do
      response <- queryRepository repository (ClaimAdmin isClaim)
      case response of
         IsOK -> done
         _ -> unpackError "setAdminStatus" response

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

unpackError :: String -> SimpleDBResponse -> a
unpackError s r = 
   let
      (errorType,mess) = case r of
         IsError errorType mess -> (errorType,mess)
         _ -> (MiscError,"but found " ++ show r)
   in
      throwError errorType ("Expecting " ++ s ++ ": " ++ mess)

----------------------------------------------------------------
-- Catching errors
----------------------------------------------------------------

-- | Detect a NotFound error and replacing it by 'Nothing'.
catchNotFound :: IO a -> IO (Maybe a)
catchNotFound act =
   do
      aOpt <- catchError 
         (do
            a <- act
            return (Just a)
            )
         (\ errorType mess -> case errorType of
            NotFoundError -> Nothing
            _ -> throwError errorType mess
            )
      seq aOpt (return aOpt)
         -- this now so throwError gets done.
{- This is the user-interface to the SimpleDB.
   -}
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
      --  -> [(Location,Maybe ObjectVersion)]
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

   CommitChange, -- = Either ObjectSource (Location,ObjectVersion)
   VersionInformation(..),

   modifyUserInfo,
      -- :: Repository -> UserInfo -> IO ()
   modifyVersionInfo,
      -- :: Repository -> VersionInfo -> IO ()
   retrieveVersionInfo,
      -- :: Repository -> ObjectVersion -> IO VersionInfo 

   getDiffs,
      -- :: Repository -> ObjectVersion -> [ObjectVersion] 
      -- -> IO [(Location,Diff)]
      -- Compare the given object version with the (presumably parent)
      -- object versions.
   Diff(..),
   ChangeData,

   queryRepository,
      -- :: Repository -> SimpleDBCommand -> IO SimpleDBResponse,
      -- General query.
   SimpleDBCommand(..),SimpleDBResponse(..),

   catchNotFound, 
      -- :: IO a -> IO (Maybe a)
      -- Catch the exception provoked by the retrieveXXX functions and
      -- getVersionInfo when a version is not found.
   catchAlreadyExists,
      -- :: IO a -> IO (Either ObjectVersion a)
      -- Catch the exception provoked by commit and modifyVersionInfo
      -- when we attempt to check in an VersionInfo which already exists
      -- in the repository, but with a different version number, or when
      -- we check in to an ObjectVersion which has already been checked in.

   catchDBError, -- :: IO a -> IO (Either String a)
   catchDBErrorWE, -- :: IO a -> IO (WithError a)
      -- These functions may be used to catch errors, when the server 
      -- returns something unexpected.
   dbError, -- :: String -> a
      -- Throw error to be caught by catchDBError.

   ) where

import System.IO.Unsafe(unsafePerformIO)

import Object
import Computation(done,WithError,toWithError,fromWithError)
import ICStringLen
import Debug(debug)
import ExtendedPrelude
import AtomString

import Destructible
import BSem
import Synchronized

import InfoBus

import HostsPorts
import HostsList(defaultUser)
import CallServer
import MultiPlexer
import PasswordFile

import CopyFile

import SimpleDBServer
import SimpleDBService
import VersionInfo hiding (server)
import ObjectSource
   -- that prevents those two functions being exported
import qualified ObjectSource


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

      initialise1 (sendCommand multiPlexer) closeDown

initialiseInternal :: VersionState -> IO Repository
initialiseInternal versionState =
   do
      simpleDB <- openSimpleDB versionState

      bSem <- newBSem

      seq defaultUser done
      let
         userId1 = defaultUser

         user = User {
            PasswordFile.userId = userId1,
            encryptedPassword = "",
            isAdmin = True,
            other = ""
            }

           
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
                  IsError mess -> dbError ("Server error: " ++ mess)
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

      importICStringLen icsl

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
   -> VersionInformation
   -> [(Location,Maybe ObjectVersion)]
   -> [(Location,CommitChange)] -> IO ()
commit repository versionInformation redirects newStuff0 =
   do
      (newStuff1 
            :: [(Location,Either ICStringLen (Location,ObjectVersion))]) <-
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
         Commit versionInformation redirects newStuff1)

      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> alreadyExistsError objectVersion
         _ -> dbError ("Commit error: unexpected response")


modifyUserInfo repository userInfo =
   do
      response 
         <- queryRepository repository (ModifyUserInfo (UserInfo1 userInfo))
      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> 
            error ("modifyUserInfo: " ++ toString objectVersion)
         _ -> dbError ("ModifyUserInfo: unexpected response")

modifyVersionInfo :: Repository -> VersionInfo -> IO ()
modifyVersionInfo repository versionInfo =
   do
      response 
         <- queryRepository repository 
            (ModifyUserInfo (VersionInfo1 versionInfo))
      case response of
         IsOK -> done
         IsObjectVersion objectVersion -> alreadyExistsError objectVersion
         _ -> dbError ("ModifyVersionInfo: unexpected response")


retrieveVersionInfo :: Repository -> ObjectVersion -> IO VersionInfo 
retrieveVersionInfo repository version =
   do
      response <- queryRepository repository (GetVersionInfo version)
      case response of
         IsVersionInfo v -> return v
         IsNotFound -> notFoundError
         _ -> dbError ("GetVersionInfo: unexpected response")


getDiffs :: Repository -> ObjectVersion -> [ObjectVersion] 
   -> IO [(Location,Diff)]
getDiffs repository version versions =
   do
      response <- queryRepository repository (GetDiffs version versions)
      case response of
         IsDiffs diffs -> return diffs
         _ -> dbError ("GetDiffs: unexpected response")

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
toData IsNotFound = notFoundError
toData r = unpackError "object" r

unpackError s r = dbError ("Expecting " ++ s ++ ": " ++ 
   case r of
      IsError mess -> mess
      _ -> " but found something else"
   ) 

----------------------------------------------------------------
-- Handling errors
----------------------------------------------------------------


-- miscellaneous errors
(dbFallOutId,catchDBError) = mkdbFallOut

catchDBErrorWE :: IO a -> IO (WithError a)
catchDBErrorWE act =
   do 
      result <- catchDBError act
      return (toWithError result)

dbError :: String -> a
dbError = mkBreakFn dbFallOutId

mkdbFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkdbFallOut #-}

-- not found exceptions
(notFoundId,catchNotFound') = mkNotFoundFallOut

catchNotFound :: IO a -> IO (Maybe a)
catchNotFound act =
   do 
      result <- catchNotFound' act
      return (case result of
         Left "" -> Nothing
         Right a -> Just a
         )

notFoundError :: a
notFoundError = mkBreakFn notFoundId ""

mkNotFoundFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkNotFoundFallOut #-}

-- alreadyExists exceptions
-- we use a horrible hack, and encode the version number in
-- the String using toString 
(alreadyExistsId,catchAlreadyExists') = mkAlreadyExistsFallOut

catchAlreadyExists :: IO a -> IO (Either ObjectVersion a)
catchAlreadyExists act =
   do
      result <- catchAlreadyExists' act
      case result of
         Left mess -> case fromWithError (fromStringWE mess) of
            Right objectVersion -> return (Left objectVersion)
         Right a -> return (Right a) 

alreadyExistsError :: ObjectVersion -> a
alreadyExistsError objectVersion 
   = mkBreakFn alreadyExistsId (toString objectVersion)

mkAlreadyExistsFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkAlreadyExistsFallOut #-}


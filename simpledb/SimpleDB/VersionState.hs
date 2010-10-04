-- | This module controls the VersionState, which records the known
-- 'VersionInfo.VersionInfo's.
module SimpleDB.VersionState(
   VersionState,

   thisServerId, -- :: VersionState -> String

   mkVersionState, -- :: Bool -> IO VersionState
      -- The Bool should be True for an internal server, False otherwise.
   addVersionInfo,
      -- :: VersionState -> VersionInfo -> IO (WithError ())
      -- Modify the VersionInfos.
   lookupVersionInfo,
      -- :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)
   lookupServerInfo,
      -- :: VersionState -> ServerInfo -> IO (Maybe ObjectVersion)
   versionIsAncestor,
      -- :: VersionState -> ObjectVersion -> ObjectVersion -> IO Bool

   getVersionInfos, -- :: VersionState -> IO [VersionInfo]
      -- get all the version infos, in undefined order.
   registerAct, -- :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
      -- Register an action to be done each time we add a new versionInfo.
      -- NB.  The action must always execute its argument exactly once.

   ) where

import Control.Monad
import Data.Maybe

import Data.IORef
import qualified Data.Map as Map
import Data.HashTable

import Util.WBFiles
import Util.Computation (done)

import Util.HostName

import Graphs.GetAncestors

import SimpleDB.VersionInfo

import SimpleDB.BDBOps
import SimpleDB.BDBExtras
import SimpleDB.ServerErrors

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

data VersionState = VersionState {
   versionDB :: BDB, -- ^ Data base of all known versions.
   serverMapRef :: IORef (Map.Map ServerInfo ObjectVersion),
      -- ^ Secondary index
   versionInfoActRef :: IORef ( IO (Bool,VersionInfo) -> IO ()),
      -- ^ Action to be performed when a VersionInfo is added (Bool is False)
      -- or modified (True), to communicate this to clients.
      --
      -- The action given in the argument must always be executed
      -- exactly once.
   parentTable :: ParentTable,
      -- This table keeps a list of the parents of a version, which
      -- we need to be able to get at for range queries in permission checks.
   thisServerId :: String
      -- ^ Unique identifier for this server.
   }

type ParentTable = HashTable Integer [Integer]

-- ----------------------------------------------------------------------
-- Creating a VersionState
-- ----------------------------------------------------------------------


-- | Create a VersionState.  The argument is 'True' if this is an internal
-- server.
mkVersionState :: Bool -> IO VersionState
mkVersionState isInternal =
   do
      versionDB <- openBDB "VersionState"
      parentTable <- Data.HashTable.new (==) fromIntegral

      versionInfos <- getBDBVersionInfos versionDB

      let
         addVersionInfo
            :: Map.Map ServerInfo ObjectVersion
            -> VersionInfo
            -> Map.Map ServerInfo ObjectVersion
         addVersionInfo map0 info =
            Map.insert (server info) (toObjectVersion info) map0

         serverMap :: Map.Map ServerInfo ObjectVersion
         serverMap = foldl addVersionInfo Map.empty versionInfos

      serverMapRef <- newIORef serverMap

      mapM_ (setParents parentTable) versionInfos

      thisServerId <- mkServerId isInternal

      versionInfoActRef <- newIORef (\ act
         -> do
               act
               done
         )

      return (VersionState {
         versionDB = versionDB,
         serverMapRef = serverMapRef,
         versionInfoActRef = versionInfoActRef,
         parentTable = parentTable,
         thisServerId = thisServerId
         })

-- ----------------------------------------------------------------------
-- Adding VersionInfo's to a version state.
-- ----------------------------------------------------------------------

-- | Add a VersionInfo to a version state, or modify it if it already
-- exists.
--
-- This function takes a transaction.  However note that if this function
-- completes, it is assumed that
addVersionInfo :: VersionState -> VersionInfo -> TXN -> IO ()
addVersionInfo versionState versionInfo1 txn =
   do
      -- We include a well-ordered check.  This means we can assume the
      -- version numbers are well-ordered for versionIsAncestor.
      if not (versionInfoIsWellOrdered versionInfo1)
         then
            throwError MiscError
               "Attempt to add VersionInfo whose parents do not precede it"
         else
            done

      let
         objectVersion :: ObjectVersion
         objectVersion = toObjectVersion versionInfo1

         setServerInfo :: VersionInfo -> IO ()
         setServerInfo versionInfo =
            atomicModifyIORef (serverMapRef versionState)
               (\ map0 -> (Map.insert (server versionInfo) objectVersion map0,()))

      versionInfo0Opt <- lookupVersionInfo versionState objectVersion

      isEdit <- case versionInfo0Opt of
         Nothing -> return False
         Just versionInfo0 ->
            do
               if server versionInfo0 /= server versionInfo1
                  then
                     throwError MiscError
                        "Attempt to change server info for a version"
                  else
                     done
               return True

      setServerInfo versionInfo1
      putVersionInfo versionState versionInfo1 txn
      setParents (parentTable versionState) versionInfo1

      versionInfoAct <- readIORef (versionInfoActRef versionState)
      versionInfoAct (return (isEdit,versionInfo1))

      done

versionInfoIsWellOrdered :: VersionInfo -> Bool
versionInfoIsWellOrdered versionInfo =
   let
      user0 = user versionInfo
   in
      all (\ parentVersion -> parentVersion < version user0) (parents user0)

-- ----------------------------------------------------------------------
-- Simple access to the versionDB
-- ----------------------------------------------------------------------

-- | Return the VersionInfo given an ObjectVersion.
lookupVersionInfo :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)
lookupVersionInfo versionState (ObjectVersion lNo) =
   do
      let
         key :: BDBKey
         key = fromIntegral lNo
      getObjectOpt (versionDB versionState) key


-- | Write the VersionInfo to the BDB (only to be used in this module).
putVersionInfo :: VersionState -> VersionInfo -> TXN -> IO ()
putVersionInfo versionState versionInfo txn =
   do
      let
         ObjectVersion lNo = toObjectVersion versionInfo

         key :: BDBKey
         key = fromIntegral lNo
      setObjectHere1 (versionDB versionState) key txn versionInfo


-- | Get all VersionInfo's in the BDB in ObjectVersion order.
-- (only to be used in this module)
getBDBVersionInfos :: BDB -> IO [VersionInfo]
getBDBVersionInfos bdb =
   do
      cursor <- mkCursor bdb
      let
         readCursor :: [VersionInfo] -> IO [VersionInfo]
         readCursor versionInfos0 =
            do
               nextOpt <- getObjectAtCursor cursor
               case nextOpt of
                  Nothing -> return (reverse versionInfos0)
                  Just (_,versionInfo) -> readCursor (
                     versionInfo : versionInfos0 )
      versionInfos <- readCursor []
      closeCursor cursor
      return versionInfos

setParents :: ParentTable -> VersionInfo -> IO ()
setParents parentTable versionInfo =
   do
      let
         objectVersion :: ObjectVersion
         objectVersion = toObjectVersion versionInfo

         parentObjectVersions :: [ObjectVersion]
         parentObjectVersions = parents . user $ versionInfo

         toInt :: ObjectVersion -> Integer
         toInt (ObjectVersion lNo) = lNo

      Data.HashTable.insert parentTable (toInt objectVersion)
         (map toInt parentObjectVersions)

-- ----------------------------------------------------------------------
-- Other miscellaneous functions
-- ----------------------------------------------------------------------

-- | Retrieve an object version given the server information.
lookupServerInfo :: VersionState -> ServerInfo -> IO (Maybe ObjectVersion)
lookupServerInfo versionState serverInfo =
   do
      serverMap <- readIORef (serverMapRef versionState)
      return (Map.lookup serverInfo serverMap)

-- | Retrieve all object versions in ObjectVersion order.
getVersionInfos :: VersionState -> IO [VersionInfo]
getVersionInfos versionState = getBDBVersionInfos (versionDB versionState)

-- | Register an action
registerAct :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
registerAct versionState actFn =
   writeIORef (versionInfoActRef versionState) actFn

-- | Returns 'True' if the first 'ObjectVersion' is an ancestor of the
-- second, or identical with it.
versionIsAncestor :: VersionState -> ObjectVersion -> ObjectVersion -> IO Bool
versionIsAncestor versionState (ObjectVersion lNo1) (ObjectVersion lNo2) =
   do
      let
         parentTable0 = parentTable versionState

         getParents lNo =
            if lNo <= lNo1
               then
                  return [] -- no point going this way
               else
                  do
                     parentsOpt <- Data.HashTable.lookup parentTable0 lNo
                     return (fromMaybe [] parentsOpt)

      isAncestor getParents lNo1 lNo2

-- ----------------------------------------------------------------------
-- Generating a unique server identifier.
-- ----------------------------------------------------------------------

-- | The argument specifies whether the server is internal or not.
mkServerId :: Bool -> IO String
mkServerId isInternal =
   do
      serverIdOpt <- getServerId
      case serverIdOpt of
         Just serverId -> return serverId
         Nothing ->
            do
               fullHostName <- getFullHostName
               if isInternal
                  then
                     do
                        return(fullHostName ++ ":#")
                  else
                     do
                        port <- getPort
                        return (if port == 11393
                           then
                              fullHostName
                           else
                              fullHostName ++ ":" ++ show port
                           )
-- ----------------------------------------------------------------------
-- Other miscellaneous functions
-- ----------------------------------------------------------------------


toObjectVersion :: VersionInfo -> ObjectVersion
toObjectVersion = version . user



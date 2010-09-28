-- | This module contains the functions for connecting to a server, closing
-- it, and listing versions.
module MMiSS.API.CallServer(
   connect,
   closeServer,
   listVersions,
   ) where

import Util.Computation

import qualified Server.PasswordFile as PasswordFile
import qualified Server.HostsPorts as HostsPorts
import Server.CallServer(tryConnect)

import Graphs.EmptyGraphSort

import qualified SimpleDB.VersionInfo as VersionInfo
import qualified Types.VersionDB as VersionDB
import Types.VersionGraph
import Types.VersionGraphClient

import MMiSS.API.Request as Request
import MMiSS.API.SessionState
import MMiSS.API.MapVersionInfo

import MMiSS.ImportExportErrors

-- ----------------------------------------------------------------------------
-- Connecting
-- --------------------------------------------------------------------------

connect :: MMiSSSessionState -> Connect -> PasswordFile.User
   -> IO ConnectResponse
connect state (Connect attrs serverRefOpt) user =
   do
      (serverStr,password)
         <- case (connectServer attrs,connectPassword attrs) of
            (Just serverStr,Just password) ->
               return (serverStr,password)
            _ -> importExportError "Server and password must both be specified!"
               -- this may change if allow no server (for the internal server)
               -- or no password (if inherited from this session or internal)
      let
         userId = case connectUser attrs of
            Nothing -> PasswordFile.userId user
            Just userId -> userId
      hostPortWE <- HostsPorts.fromHostDescription1 serverStr
         (HostsPorts.LoginInfo {
            HostsPorts.user = userId,
            HostsPorts.password = password
            })

      hostPort <- coerceWithErrorOrBreakIO importExportError hostPortWE
      versionGraph <-
         let
            ?server = hostPort
         in
            do
               errOrRepository <- tryConnect VersionDB.initialise
               repository <- case errOrRepository of
                  Left err -> importExportError err
                  Right repository -> return repository

               newVersionGraph emptyGraphSort repository

      serverRef <- setServer state serverRefOpt versionGraph
      return (ConnectResponse serverRef)

-- --------------------------------------------------------------------------
-- Closing
-- --------------------------------------------------------------------------

closeServer :: MMiSSSessionState -> CloseServer -> IO CloseServerResponse
closeServer state (CloseServer serverRef) =
   do
      deleteVersionGraph state serverRef
      return CloseServerResponse


-- --------------------------------------------------------------------------
-- Listing versions
-- --------------------------------------------------------------------------

listVersions :: MMiSSSessionState -> ListVersions -> IO ListVersionsResponse
listVersions state (ListVersions serverRef) =
   do
      versionGraph <- lookupVersionGraph state serverRef
      let
         graphClient :: VersionGraphClient
         graphClient = toVersionGraphClient versionGraph

      (versionInfos1 :: [VersionInfo1])
         <- getVersionInfos graphClient
      let
         versionInfos2 :: [VersionInfo.VersionInfo]
         versionInfos2 = map toVersionInfo versionInfos1

         versionInfos3 :: [Request.VersionInfo]
         versionInfos3 = map fromOurVersionInfo versionInfos2

      return (ListVersionsResponse versionInfos3)

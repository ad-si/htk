{- This module contains the functions for connecting to a server, closing 
   it, and listing versions. -}
module MMiSSCallServer(
   connect,
   closeServer,
   listVersions,
   ) where

import Messages
import Computation
import AtomString
import ExtendedPrelude
import ClockTimeToString

import Destructible

import qualified PasswordFile
import qualified HostsPorts
import CallServer(tryConnect)

import Graph
import EmptyGraphSort

import Text.XML.HaXml.Xml2Haskell

import qualified VersionInfo
import qualified VersionDB
import VersionGraph
import VersionGraphClient

import MMiSSRequest
import MMiSSSessionState

import {-# SOURCE #-} MMiSSDoXml

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
            _ -> ourError "Server and password must both be specified!"
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

      hostPort <- coerceWithErrorOrBreakIO ourError hostPortWE
      versionGraph <-
         let
            ?server = hostPort
         in
            do
               errOrRepository <- tryConnect VersionDB.initialise
               repository <- case errOrRepository of
                  Left err -> ourError err
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
         simpleGraph :: VersionSimpleGraph 
         simpleGraph = toVersionGraphGraph versionGraph

      (nodes :: [Node]) <- getNodes simpleGraph
      (versionInfos0 :: [VersionInfo.VersionInfo]) 
         <- mapM (getNodeLabel simpleGraph) nodes
      let
         mapVersionInfo :: VersionInfo.VersionInfo 
            -> MMiSSRequest.VersionInfo
         mapVersionInfo versionInfo0 =
            let
               versionInfoIsPresent1 = 
                  if VersionInfo.isPresent versionInfo0
                     then
                        Default VersionInfo_isPresent_present
                     else
                        NonDefault VersionInfo_isPresent_absent

               user0 = VersionInfo.user versionInfo0

               private1 = if VersionInfo.private user0
                  then
                     NonDefault UserInfo_private_noAutoExport
                  else
                     Default UserInfo_private_autoExport

               user1 = UserInfo {
                  userInfoLabel = Just (VersionInfo.label user0),
                  userInfoContents = Just (VersionInfo.contents user0),
                  userInfoPrivate = private1,
                  userInfoVersion 
                     = Just (toString (VersionInfo.version user0)),
                  userInfoParents 
                     = Just (unsplitByChar0 ' '
                        (map toString (VersionInfo.parents user0)))
                     }

               server0 = VersionInfo.server versionInfo0

               server1 = ServerInfo {
                  serverInfoServerId = VersionInfo.serverId server0,
                  serverInfoSerialNo = show (VersionInfo.serialNo server0),
                  serverInfoTimeStamp 
                     = clockTimeToString (VersionInfo.timeStamp server0),
                  serverInfoUserId = VersionInfo.userId server0
                  }
            in
               VersionInfo 
                  (VersionInfo_Attrs {
                     versionInfoIsPresent = versionInfoIsPresent1})
                  user1 server1

         versionInfos1 = map mapVersionInfo versionInfos0

      return (ListVersionsResponse versionInfos1)
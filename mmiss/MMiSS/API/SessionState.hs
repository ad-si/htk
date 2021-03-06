-- | This module defines the state which is preserved for a user during a
-- session.  It also defines various utilities for manipulating it.
module MMiSS.API.SessionState(
   MMiSSSessionState(..),
   MMiSSSessionStateValue(..),
   newSessionState, -- :: IO MMiSSSessionState
   readState, -- :: MMiSSSessionState -> (MMiSSSessionStateValue -> a) -> IO a

   lookupVersionGraph,
      -- :: MMiSSSessionState -> ServerRef -> IO VersionGraph
   deleteVersionGraph,
      -- :: MMiSSSessionState -> ServerRef -> IO ()
   setServer,
      -- :: MMiSSSessionState -> Maybe ServerRef -> VersionGraph
      -- -> IO ServerRef

   lookupView,
      -- :: MMiSSSessionState -> VersionRef -> IO View
   deleteView,
      -- :: MMiSSSessionState -> VersionRef -> IO ()
   setView,
      -- :: MMiSSSessionState -> Maybe VersionRef -> View
      -- -> IO VersionRef



   ) where

import Control.Concurrent.MVar
import qualified Data.Map as Map

import Util.UniqueString
import Util.Computation

import Events.Destructible

import Types.VersionGraph
import Types.ViewType

import Text.XML.HaXml.Xml2Haskell

import MMiSS.ImportExportErrors

import MMiSS.API.Request
import MMiSS.API.Basics

-- --------------------------------------------------------------------------
-- Datatypes
-- --------------------------------------------------------------------------

newtype MMiSSSessionState = MMiSSSessionState (MVar MMiSSSessionStateValue)

data MMiSSSessionStateValue = MMiSSSessionStateValue {
   messages :: Messages, -- ^ Messages are in reverse order.
   servers :: Map.Map ServerRef VersionGraph,
   versions :: Map.Map VersionRef View,
   serverRefSource :: UniqueStringCounter,
   versionRefSource :: UniqueStringCounter
   }


-- --------------------------------------------------------------------------
-- Creating a new session state
-- --------------------------------------------------------------------------

newSessionState :: IO MMiSSSessionState
newSessionState =
   do
      mVar <- newMVar initialSessionState
      return (MMiSSSessionState mVar)


initialSessionState :: MMiSSSessionStateValue
initialSessionState = MMiSSSessionStateValue {
   messages = Messages
      (Messages_Attrs {
         messagesStatus = Default Messages_status_success
         })
      [],
   servers = Map.empty,
   versions = Map.empty,
   serverRefSource = firstUniqueStringCounter,
   versionRefSource = firstUniqueStringCounter
   }

-- -------------------------------------------------------------------------
-- Server refs
-- -------------------------------------------------------------------------

lookupVersionGraph :: MMiSSSessionState -> ServerRef -> IO VersionGraph
lookupVersionGraph state serverRef =
   do
      servers0 <- readState state servers
      case Map.lookup serverRef servers0 of
         Nothing -> importExportError "Server not known"
         Just versionGraph -> return versionGraph

deleteVersionGraph :: MMiSSSessionState -> ServerRef -> IO ()
deleteVersionGraph (MMiSSSessionState mVar) serverRef =
   modifyMVar_ mVar
      (\ state ->
         do
            let
               servers0 = servers state
            case Map.lookup serverRef servers0 of
               Nothing -> importExportError "Server not known"
               Just versionGraph -> destroy versionGraph
            let
               servers1 = Map.delete serverRef servers0
            return (state {servers = servers1})
         )

setServer :: MMiSSSessionState -> Maybe ServerRef -> VersionGraph
   -> IO ServerRef
setServer (MMiSSSessionState mVar) serverRefOpt versionGraph =
   modifyMVar mVar
      (\ state0 ->
         do
            let
               (state1,serverRef) = case serverRefOpt of
                  Just serverRef -> (state0,serverRef)
                  Nothing ->
                     let
                        (serverRef,serverRefSource1) =
                           newRefGen
                              (\ str -> ServerRef {serverRefRef = str})
                              (servers state0)
                              (serverRefSource state0)
                     in
                        (state0 {serverRefSource = serverRefSource1},serverRef)

               servers0 = servers state1

            case Map.lookup serverRef servers0 of
               Nothing -> done
               Just versionGraph0 -> destroy versionGraph0

            let
               servers1 = Map.insert serverRef versionGraph servers0

               state2 = state1 {servers = servers1}
            return (state2,serverRef)
         )

-- -------------------------------------------------------------------------
-- Version refs
-- -------------------------------------------------------------------------

lookupView :: MMiSSSessionState -> VersionRef -> IO View
lookupView state versionRef =
   do
      versions0 <- readState state versions
      case Map.lookup versionRef versions0 of
         Nothing -> importExportError "Version not known"
         Just view -> return view

deleteView :: MMiSSSessionState -> VersionRef -> IO ()
deleteView (MMiSSSessionState mVar) versionRef =
   modifyMVar_ mVar
      (\ state ->
         do
            let
               versions0 = versions state
            case Map.lookup versionRef versions0 of
               Nothing -> importExportError "Version not known"
               Just versionGraph -> destroy versionGraph
            let
               versions1 = Map.delete versionRef versions0
            return (state {versions = versions1})
         )


setView :: MMiSSSessionState -> Maybe VersionRef -> View
   -> IO VersionRef
setView (MMiSSSessionState mVar) versionRefOpt view =
   modifyMVar mVar
      (\ state0 ->
         do
            let
               (state1,versionRef) = case versionRefOpt of
                  Just versionRef -> (state0,versionRef)
                  Nothing ->
                     let
                        (versionRef,versionRefSource1) =
                           newRefGen
                              (\ str -> VersionRef {versionRefRef = str})
                              (versions state0)
                              (versionRefSource state0)
                     in
                        (state0 {versionRefSource = versionRefSource1},
                           versionRef)

               versions0 = versions state1

            case Map.lookup versionRef versions0 of
               Nothing -> done
               Just view1 -> destroy view1

            let
               versions1 = Map.insert versionRef view versions0

               state2 = state1 {versions = versions1}
            return (state2,versionRef)
         )

-- -------------------------------------------------------------------------
-- Allocating new references
-- -------------------------------------------------------------------------

newRefGen :: Ord key => (String -> key) -> Map.Map key value
   -> UniqueStringCounter -> (key,UniqueStringCounter)
newRefGen mkKey map0 counter0 =
   let
      (s0,counter1) = stepUniqueStringCounter counter0

      key0 = mkKey s0
   in
      if Map.member key0 map0
         then
            newRefGen mkKey map0 counter1
         else
            (key0,counter1)

-- --------------------------------------------------------------------------
-- Utility functions
-- --------------------------------------------------------------------------

readState :: MMiSSSessionState -> (MMiSSSessionStateValue -> a) -> IO a
readState (MMiSSSessionState mVar) getVal =
   do
      state <- readMVar mVar
      return (getVal state)

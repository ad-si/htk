{- This module defines the state which is preserved for a user during a 
   session.  It also defines various utilities for manipulating it. -}
module MMiSSSessionState(
   MMiSSSessionState(..),
   MMiSSSessionStateValue(..),
   newSessionState, -- :: IO MMiSSSessionState
   readState, -- :: MMiSSSessionState -> (MMiSSSessionStateValue -> a) -> IO a
   ) where

import Control.Concurrent.MVar
import Data.FiniteMap

import UniqueString

import VersionGraph
import ViewType

import Text.XML.HaXml.Xml2Haskell

import MMiSSRequest
import MMiSSAPIBasics

-- --------------------------------------------------------------------------
-- Datatypes
-- --------------------------------------------------------------------------

newtype MMiSSSessionState = MMiSSSessionState (MVar MMiSSSessionStateValue)

data MMiSSSessionStateValue = MMiSSSessionStateValue {
   messages :: Messages, -- ^ Messages are in reverse order.
   servers :: FiniteMap ServerRef VersionGraph,
   versions :: FiniteMap VersionRef View,
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
   servers = emptyFM,
   versions = emptyFM,
   serverRefSource = firstUniqueStringCounter,
   versionRefSource = firstUniqueStringCounter
   }

-- --------------------------------------------------------------------------
-- Utility functions
-- --------------------------------------------------------------------------

readState :: MMiSSSessionState -> (MMiSSSessionStateValue -> a) -> IO a
readState (MMiSSSessionState mVar) getVal =
   do
      state <- readMVar mVar
      return (getVal state)
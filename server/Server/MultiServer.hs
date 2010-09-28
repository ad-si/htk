-- | This is a different abstraction to the standard server package, designed
-- for the Emacs encapsulation.
module Server.MultiServer(
   MultiServer,

   newMultiServer, -- :: Bool -> (Maybe PortNumber) -> IO MultiServer
   -- We only accept clients from machines other than this one
   -- if the Bool is True.

   -- If the PortNumber is not supplied we make one up.

   getPortNumber, -- :: MultiServer -> IO PortNumber
   -- Get the actual port being used by this MultiServer

   MultiServerKey, -- Key identifying a particular client.

   newMultiServerKey, -- :: MultiServer -> IO MultiServerKey
   -- Generate a new unique MultiServerKey.

   fromMultiServerKey, -- :: MultiServerKey -> String
   -- The client is expected to send this String followed by a newline
   -- at the very start of the session.

   waitForClient, -- :: MultiServer -> MultiServerKey -> IO () -> IO Handle
   -- This picks up a client sending a particular MultiServerKey, performing
   -- the action (which presumably starts the client off) while waiting.
   ) where

import Maybe
import IO
import Random

import Network
import qualified Network.Socket
import qualified Network.BSD
import Control.Concurrent

import Util.Registry
import Util.Object

-- ------------------------------------------------------------------------
-- The datatypes
-- ------------------------------------------------------------------------

data MultiServer = MultiServer {
   socket :: Socket,

   -- returns True if we accept clients from this host
   hostNameFilter :: String -> IO Bool,

   -- map to MVar for unsatisfied requests
   awaitedClients :: Registry String (MVar Handle)
   }

newtype MultiServerKey = MultiServerKey String


-- ------------------------------------------------------------------------
-- Starting a MultiServer.  (For the time being there is no provision for
-- stopping one.)
-- ------------------------------------------------------------------------

newMultiServer :: Bool -> (Maybe PortNumber) -> IO MultiServer
newMultiServer acceptNonLocal portNumberOpt =
   do
      let
         portNumber = fromMaybe Network.Socket.aNY_PORT portNumberOpt
      socket <- listenOn (PortNumber portNumber)

      thisHostName <- Network.BSD.getHostName
      thisHostEntry <- Network.BSD.getHostByName thisHostName
      let
         thisHostAddress = Network.BSD.hostAddress thisHostEntry
      localHostAddress <- Network.Socket.inet_addr "127.0.0.1"

      let
         hostNameFilter otherHostName =
            if acceptNonLocal then return True else
               do
                  otherHostEntry <- Network.BSD.getHostByName otherHostName
                  let
                     otherHostAddress = Network.BSD.hostAddress otherHostEntry
                  return (otherHostAddress == localHostAddress ||
                     otherHostAddress == thisHostAddress
                     )

      awaitedClients <- newRegistry
      let
         multiServer =
            MultiServer {
               socket = socket,
               hostNameFilter = hostNameFilter,
               awaitedClients = awaitedClients
               }
      forkIO (workerThread multiServer)
      return multiServer

workerThread :: MultiServer -> IO ()
workerThread multiServer =
   do
      (handle,hostName,_) <- accept (socket multiServer)
      acceptThisHost <- hostNameFilter multiServer hostName
      if acceptThisHost
         then
            do
               key <- hGetLine handle
               transformValue (awaitedClients multiServer) key
                  (\ mVarOpt ->
                     do
                        case mVarOpt of
                           Nothing -> putStrLn (
                                 "MultiServer: Unexpeced client with key "++
                                 show key)
                           Just mVar -> putMVar mVar handle
                        return (Nothing,())
                     )
         else
            putStrLn ("MultiServer: Attempt to connect from "++hostName++
               " rejected")
      workerThread multiServer

-- ------------------------------------------------------------------------
-- Other functions on MultiServer's.
-- ------------------------------------------------------------------------

getPortNumber :: MultiServer -> IO PortNumber
getPortNumber multiServer = Network.Socket.socketPort (socket multiServer)

waitForClient :: MultiServer -> MultiServerKey -> IO () -> IO Handle
waitForClient multiServer multiServerKey action =
   do
      mVar <- newEmptyMVar
      setValue (awaitedClients multiServer)
         (fromMultiServerKey multiServerKey) mVar
      action
      takeMVar mVar

-- ------------------------------------------------------------------------
-- MultiServerKey's.
-- ------------------------------------------------------------------------

fromMultiServerKey :: MultiServerKey -> String
fromMultiServerKey (MultiServerKey str) = str

-- We try to make these fairly unguessable though they are hardly
-- cryptographically secure.
newMultiServerKey :: MultiServer -> IO MultiServerKey
newMultiServerKey _ =
   do
      i1 <- newInt -- this forces uniqueness
      (i2 :: Int) <- randomIO
      return (MultiServerKey (show i1 ++ "." ++ show i2))

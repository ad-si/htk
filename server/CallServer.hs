{- CallServer does the job of calling up a server.  -}
module CallServer(
   -- The context of all these functions is the same and is omitted from
   -- the following signature declarations.
   -- (?server :: HostPort,ServiceClass inType outType stateType,
   --       HasBinaryIO header)


   connectReply, -- :: 
      --  (inType,outType,stateType) -> IO (inType -> IO outType,IO (),header)
      -- connectReply should be used for Reply-type services.  It
      -- attempts to connect to the server.
      -- If successful it returns a tuple consisting
      -- (a) of an action which converts inType to outType by calling the
      --     server;
      -- (b) an action which will break the connection.
      -- (c) the header string sent by sendOnConnect.
   connectBroadcast, -- ::
      --  (inType,outType,stateType) ->    
      --     IO (inType -> IO (),IO outType,IO (),header)
      -- connectBroadcast should be used for Broadcast-type services.
      -- It attempts to connect to the server.
      -- If successful it returns a tuple containing
      -- (a) a function generating an action for sending inType to the server;
      -- (b) an action which waits for the next outType from the server;
      -- (c) an action for closing the connection.
      -- (d) the header string sent by sendOnConnect
   connectBroadcastOther, -- ::
      -- Identical to connectBroadcast except it connects to a 
      -- BroadcastOther-type service.
   connectExternal, -- ::
      -- (inType,outType,stateType) -> IO (IO outType,IO (),header)
      -- Connect to an External service.

   tryConnect, 
      -- :: IO a -> IO (Either String a)
      -- Wrap round an operation that tries to connect to a server;
      -- this catches connection failures (specifically, when the user
      -- cancels when asked for a userid/password).

   ) where

import IO

import Control.Concurrent.MVar
import System.IO.Unsafe

import Computation
import Debug(debug)
import Object
import WBFiles
import BinaryIO
import ExtendedPrelude

import Destructible

import SimpleForm
import DialogWin

import HostsPorts

import BSem
import InfoBus

import ServiceClass
import HostsList

connectReply :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinaryIO header)
   => (inType,outType,stateType)  
   -> IO (inType -> IO outType,IO (),header)
connectReply service =
   do
      case (serviceMode service) of
         Reply -> done
         _ -> ioError(userError("connectReply handed a non-Reply service"))
      (connection@ Connection {handle = handle}) <- connectBasic service

      header <- hGet handle

      bSem <- newBSem 
      let
         sendMessage inData =
            synchronize bSem (
               do
                  hPut handle inData
                  hFlush handle
                  hGet handle
               )
         closeAct = destroy connection
      return (sendMessage,closeAct,header)
   
connectBroadcast :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinaryIO header)
   => (inType,outType,stateType)     
   -> IO (inType -> IO (),IO outType,IO (),header)
connectBroadcast service =
   do
      case (serviceMode service) of
         Broadcast -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      connectBroadcastGeneral service

connectBroadcastOther :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinaryIO header)
   => (inType,outType,stateType) ->     
      IO (inType -> IO (),IO outType,IO (),header)
connectBroadcastOther service =
   do
      case (serviceMode service) of
         BroadcastOther -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      connectBroadcastGeneral service

connectExternal :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinaryIO header)
   => (inType,outType,stateType) 
   -> IO (IO outType,IO (),header)
connectExternal service =
   do
      case (serviceMode service) of
         External _ -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      (_,getNext,closeDown,header) <- connectBroadcastGeneral service
      return (getNext,closeDown,header)

connectBroadcastGeneral :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinaryIO header)
   => (inType,outType,stateType)     
   -> IO (inType -> IO (),IO outType,IO (),header)
connectBroadcastGeneral service =
   do
      (connection@ Connection {handle = handle}) <- connectBasic service

      header <- hGet handle

      readBSem <- newBSem
      writeBSem <- newBSem      

      let
         sendMessage inData =
            synchronize readBSem (
               do
                  hPut handle inData
                  hFlush handle
               )
         getMessage = synchronize writeBSem (hGet handle)
         closeAct = destroy connection

      return (sendMessage,getMessage,closeAct,header)

------------------------------------------------------------------------
-- A Connection is what we register so that shutdown closes the
-- connection.
------------------------------------------------------------------------

data Connection = Connection {
   handle :: Handle,
   oId :: ObjectID
   }

newConnection :: Handle -> IO Connection
newConnection handle =
   do
      oId <- newObject
      return (Connection {
         handle = handle,
         oId = oId
         })

instance Object Connection where
   objectID (Connection {oId = oId}) = oId
   
instance Destroyable Connection where
   destroy (connection@Connection {handle = handle}) =
      do
         deregisterTool connection
         hClose handle

------------------------------------------------------------------------
-- getHost is used by all of them
------------------------------------------------------------------------

getHost :: IO String
getHost =
   do
      hostOpt <- getServer
      case hostOpt of
         Nothing -> error "CallServer: server unset"
         Just host -> return host

------------------------------------------------------------------------
-- This function does the work of opening a connection, sending the userId
-- and password, and so on.
------------------------------------------------------------------------

connectBasic :: 
   (?server :: HostPort,
      ServiceClass inType outType stateType)
   => (inType,outType,stateType) -> IO Connection
connectBasic service =
   do
      let
         serviceKey = serviceId service

         -- This function iterates attempting to get a connection.
         -- The Bool is True the first time it is called.
         connectBasic :: Bool -> IO Connection
         connectBasic firstTime =
            do    
               handle <- connect

               hSetBuffering handle (BlockBuffering (Just 4096))
                  -- since we may well be doing the connection via SSL,
                  -- we use a big buffer, and only flush when necessary.

               userPasswordOpt <- getUserPassword (not firstTime) (?server)
               (user,password) <- case userPasswordOpt of
                  Nothing -> connectFailure "Server connection cancelled"
                  Just (user,password) -> return (user,password)

               hPut handle serviceKey
               hPut handle user
               hPut handle password
               hFlush handle

               response <- hGetLine handle 
               case response of
                  "OK" -> 
                     do
                        connection <- newConnection handle
                        registerTool connection
                        return connection
                  errorMess ->
                     do
                        hClose handle
                        createErrorWin
                           ("Server rejected connection: " ++ errorMess) []
                        connectBasic False
              
      connectBasic True

-- -------------------------------------------------------------------
-- Error functions.
-- -------------------------------------------------------------------

tryConnect :: IO a -> IO (Either String a)

connectFailure = mkBreakFn connectFailureId

(connectFailureId,tryConnect) = mkConnectFallOut

mkConnectFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkConnectFallOut #-}



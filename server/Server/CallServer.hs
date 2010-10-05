{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoMonoPatBinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CallServer does the job of calling up a server.
module Server.CallServer(
   -- The context of all these functions is the same and is omitted from
   -- the following signature declarations.
   -- (?server :: HostPort,ServiceClass inType outType stateType,
   --       HasBinary header IO)


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

import System.IO

import System.IO.Unsafe

import Util.Computation
import Util.Object
import Util.ExtendedPrelude
import Util.BinaryAll
import Util.Messages

import Events.Destructible
import Events.Synchronized

import Server.HostsPorts

import Reactor.BSem
import Reactor.InfoBus

import Server.ServiceClass
import Server.HostsList

connectReply ::
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinary header IO)
   => (inType,outType,stateType)
   -> IO (inType -> IO outType,IO (),header)
connectReply service =
   do
      case (serviceMode service) of
         Reply -> done
         _ -> ioError(userError("connectReply handed a non-Reply service"))
      (connection@ Connection {handle = handle}) <- connectBasic service

      header <- hRead handle

      bSem <- newBSem

      let
         sendMessage inData =
            synchronize bSem (
               do
                  hWrite handle inData
                  hFlush handle
                  hRead handle
               )
         closeAct = destroy connection
      return (sendMessage,closeAct,header)

connectBroadcast ::
   (?server :: HostPort,
      ServiceClass inType outType stateType,HasBinary header IO)
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
      ServiceClass inType outType stateType,HasBinary header IO)
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
      ServiceClass inType outType stateType,HasBinary header IO)
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
      ServiceClass inType outType stateType,HasBinary header IO)
   => (inType,outType,stateType)
   -> IO (inType -> IO (),IO outType,IO (),header)
connectBroadcastGeneral service =
   do
      (connection@ Connection {handle = handle}) <- connectBasic service

      header <- hRead handle

      readBSem <- newBSem
      writeBSem <- newBSem

      let
         sendMessage inData =
            synchronize readBSem (
               do
                  hWrite handle inData
                  hFlush handle
               )
         getMessage = synchronize writeBSem (hRead handle)
         closeAct = destroy connection

      return (sendMessage,getMessage,closeAct,header)

------------------------------------------------------------------------
-- A Connection is what we register so that shutdown closes the
-- connection.
------------------------------------------------------------------------

data Connection = Connection {
   handle :: Handle,
   oId :: ObjectID,
   destroyAct :: IO ()
   }

newConnection :: Handle -> IO Connection
newConnection handle =
   mdo
      oId <- newObject
      connection <-
         do
            destroyAct <- doOnce (destroySimple connection)
            return (Connection {
               handle = handle,
               oId = oId,
               destroyAct = destroyAct
               })
      return connection

instance Object Connection where
   objectID (Connection {oId = oId}) = oId

destroySimple :: Connection -> IO ()
destroySimple (connection@Connection {handle = handle}) =
   do
      deregisterTool connection
      -- hClose handle
      -- doesn't seem to terminate for some reason.

instance Destroyable Connection where
   destroy = destroyAct

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

               (user,password,tryAgain) <- case toLoginInfo ?server of
                  Just loginInfo ->
                     return (user loginInfo,password loginInfo,False)
                  Nothing ->
                     do
                        userPasswordOpt
                           <- getUserPassword (not firstTime) (?server)
                        case userPasswordOpt of
                           Nothing -> connectFailure
                              "Server connection cancelled"
                           Just (user,password) -> return (user,password,True)

               hWrite handle (serviceKey,user,password)
               hFlush handle

               response <- hGetLine handle
               case response of
                  "OK" ->
                     do
                        connection <- newConnection handle
                        registerToolDebug ("callServer:" ++ serviceKey)
                           connection
                        return connection
                  mess ->
                     do
                        hClose handle
                        errorMess ("Server rejected connection: " ++ mess)
                        if tryAgain
                           then
                              connectBasic False
                           else
                              connectFailure "Authentication failed"
      connectBasic True


-- -------------------------------------------------------------------
-- Error functions.
-- -------------------------------------------------------------------

tryConnect :: IO a -> IO (Either String a)
connectFailure :: String -> a
connectFailureId :: ObjectID
mkConnectFallOut :: (ObjectID,IO a -> IO (Either String a))

connectFailure = mkBreakFn connectFailureId
(connectFailureId,tryConnect) = mkConnectFallOut

mkConnectFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkConnectFallOut #-}



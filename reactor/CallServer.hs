{- CallServer does the job of calling up a server.  This really ought
   to be in the server/ directory, except that it uses InfoBus. -}
module CallServer(
   connectReply, -- :: 
      -- (ServiceClass inType outType stateType) =>
      --    (inType,outType,stateType) ->     
      --       IO (inType -> IO outType,IO (),String)
      -- connectReply should be used for Reply-type services.  It
      -- attempts to connect to the server.
      -- If successful it returns a tuple consisting
      -- (a) of an action which converts inType to outType by calling the
      --     server;
      -- (b) an action which will break the connection.
      -- (c) the header string sent by sendOnConnect.
   connectBroadcast, -- ::
      -- (ServiceClass inType outType stateType) =>
      --    (inType,outType,stateType) ->    
      --       IO (inType -> IO (),IO outType,IO (),String)
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
   ) where

import IO

import Computation
import Debug(debug)
import Object
import WBFiles

import Destructible

import HostsPorts

import BSem
import InfoBus

import ServiceClass

connectReply :: (ServiceClass inType outType stateType) =>
    (inType,outType,stateType) ->  
    IO (inType -> IO outType,IO (),String)
connectReply service =
   do
      case (serviceMode service) of
         Reply -> done
         _ -> ioError(userError("connectReply handed a non-Reply service"))

      hostDesc <- getHost
      portDesc <- getPort
      handle <- connect hostDesc portDesc
      connection <- newConnection handle
      registerTool connection

      let
         serviceKey = serviceId service
      hPutStrLn handle serviceKey

      headerLine <- hGetLine handle
      let
         header = read headerLine

      bSem <- newBSem 
      let
         sendMessage inData =
            do
               let 
                  inLine = show inData
               outLine <-
                  synchronize bSem (
                     do
                        hPutStrLn handle inLine
                        hGetLine handle
                     )
               return (read outLine)
         closeAct = destroy connection
      return (sendMessage,closeAct,header)
   
connectBroadcast :: (ServiceClass inType outType stateType) =>
      (inType,outType,stateType) ->     
      IO (inType -> IO (),IO outType,IO (),String)
connectBroadcast service =
   do
      case (serviceMode service) of
         Broadcast -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      connectBroadcastGeneral service

connectBroadcastOther :: (ServiceClass inType outType stateType) =>
      (inType,outType,stateType) ->     
      IO (inType -> IO (),IO outType,IO (),String)
connectBroadcastOther service =
   do
      case (serviceMode service) of
         BroadcastOther -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      connectBroadcastGeneral service

connectBroadcastGeneral :: (ServiceClass inType outType stateType) =>
      (inType,outType,stateType) ->     
      IO (inType -> IO (),IO outType,IO (),String)
connectBroadcastGeneral service =
   do
      hostDesc <- getHost
      portDesc <- getPort
      handle <- connect hostDesc portDesc
      connection <- newConnection handle
      registerTool connection

      let
         serviceKey = serviceId service
      hPutStrLn handle serviceKey

      headerLine <- hGetLine handle
      let
         header = read headerLine

      readBSem <- newBSem
      writeBSem <- newBSem      

      let
         sendMessage inData =
            do
               let
                  inLine = show inData
               synchronize readBSem (hPutStrLn handle inLine)
               debug ("Sent "++inLine)
         getMessage =
            do
               outLine <- synchronize writeBSem (hGetLine handle)
               debug ("Received "++outLine)
               return (read outLine)
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



{- CallServer does the job of calling up a server -}
module CallServer(
   connectReply, -- :: 
      -- (ServiceClass inType outType stateType,DescribesHost host,
      --    DescribesPort port) =>
      --    (inType,outType,stateType) -> host -> port ->     
      --       IO (inType -> IO outType,IO ())
      -- connectReply should be used for Reply-type services.  It
      -- attempts to connect to a server with the supplied
      -- host and port.  If successful it returns a tuple consisting
      -- (a) of an action which converts inType to outType by calling the
      --     server;
      -- (b) an action which will break the connection.  
   connectBroadcast, -- ::
      -- (ServiceClass inType outType stateType,DescribesHost host,
      --    DescribesPort port) =>
      --    (inType,outType,stateType) -> host -> port ->     
      --       IO (inType -> IO (),IO outType,IO ())
      -- connectBroadcast should be used for Broadcast-type services.
      -- It attempts to connect to the server with supplied host and port.
      -- If successful it returns a tuple containing
      -- (a) a function generating an action for sending inType to the server;
      -- (b) an action which waits for the next outType from the server;
      -- (c) an action for closing the connection.
   ) where

import IO

import Computation
import Debug(debug)

import BSem
import SocketEV

import ServiceClass

connectReply :: (ServiceClass inType outType stateType,DescribesHost host,
      DescribesPort port) =>
    (inType,outType,stateType) -> host -> port ->  
    IO (inType -> IO outType,IO ())
connectReply service hostDesc portDesc =
   do
      case (serviceMode service) of
         Reply -> done
         _ -> ioError(userError("connectReply handed a non-Reply service"))
      handle <- connect hostDesc portDesc
      let
         serviceKey = serviceId service
      hPutStrLn handle serviceKey
      bSem <- newBSem 
      let
         sendMessage inData =
            do
               let 
                  inLine = show inData
               outLine <-
                  synchronize bSem (
                     do
                        hPutStrLn handle (show inLine)
                        hGetLine handle
                     )
               return (read outLine)
         closeAct = hClose handle
      return (sendMessage,closeAct)
   
connectBroadcast :: (ServiceClass inType outType stateType,DescribesHost host,
      DescribesPort port) =>
      (inType,outType,stateType) -> host -> port ->     
      IO (inType -> IO (),IO outType,IO ())
connectBroadcast service hostDesc portDesc =
   do
      case (serviceMode service) of
         Broadcast -> done
         _ -> ioError(userError(
           "connectBroadcast handed a non-Broadcast service"))
      handle <- connect hostDesc portDesc
      let
         serviceKey = serviceId service
      hPutStrLn handle serviceKey
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
         closeAct = hClose handle
      return (sendMessage,getMessage,closeAct)

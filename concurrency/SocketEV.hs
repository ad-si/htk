{- SocketEV provides a simple encapsulation of the GHC Socket interface
   (and therefore BSD sockets) in terms of events.  Communication
   is via the tcp interface in line mode.  (So strings should not
   contain newlines.)
   -}
module SocketEV(
   -- general functions
   HostDesc, -- description of the host, instance of Show
   DescribesHost(makeHost), -- :: a -> IO HostDesc
      -- defined for Strings
   PortDesc, -- description of the port, instance of Show
   DescribesPort(makePort), -- :: a -> IO PortDesc
      -- defined for Int

   -- functions for a server
   listenEV, -- :: PortDesc -> IO(EV(HostDesc,PortDesc,HandleEV))
   -- listenEV should only be called once per port.  It returns
   -- an event which happens when new clients attempt to call,
   -- giving the clients host, port and a HandleEV for accessing it.

   -- functions for a client
   connect -- :: HostDesc -> PortDesc -> IO HandleEV
   -- connect attempts to connect to the server
   ) where

import Socket
import Channels
import EV
import FileEV
import Thread

newtype HostDesc = HostDesc String deriving Show

class DescribesHost a where
   makeHost :: a -> IO HostDesc

instance DescribesHost String where
   makeHost name = return (HostDesc name)

newtype PortDesc = PortDesc Int deriving Show

descToPort (PortDesc port) = PortNumber(fromIntegral port)   

class DescribesPort a where
   makePort :: a -> IO PortDesc

instance DescribesPort Int where
   makePort name = return (PortDesc name)   


listenEV :: PortDesc -> IO(EV(HostDesc,PortDesc,HandleEV))
listenEV portDesc =
   do
      socket <- listenOn(descToPort portDesc)
      newConnections <- 
         newChannel :: (IO (Channel (HostDesc,PortDesc,HandleEV)))
      let
         listenerThread = 
            do
               (handle,hostName,portNumber) <- accept socket
               let
                  hostDesc = HostDesc hostName
                  portDesc = PortDesc (fromIntegral portNumber)
               handleEV <- makeFileEV handle
               sendIO newConnections (hostDesc,portDesc,handleEV)
               listenerThread
      forkIO listenerThread
      return (receive newConnections) 
               
connect :: HostDesc -> PortDesc -> IO HandleEV
connect (HostDesc hostName) portDesc =
   do
      handle <- connectTo hostName (descToPort portDesc)
      makeFileEV handle


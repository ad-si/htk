{- SocketEV provides a simple encapsulation of the GHC Socket interface
   (and therefore BSD sockets) in terms of events.  Communication
   is via the tcp interface in line mode.  (So strings should not
   contain newlines.)
   -}
module SocketEV(
   -- general functions
   HostDesc, -- description of the host, instance of Show
   DescribesHost(makeHost), -- :: a -> IO HostDesc
      -- defined for Strings and HostDesc
   PortDesc, -- description of the port, instance of Show
   DescribesPort(makePort), -- :: a -> IO PortDesc
      -- defined for Int and PortDesc

   -- functions for a server
   listenEV, -- :: DescribesPort port => port 
             --      -> IO(EV(HostDesc,PortDesc,HandleEV))
   -- listenEV should only be called once per port.  It returns
   -- an event which happens when new clients attempt to call,
   -- giving the clients host, port and a HandleEV for accessing it.

   -- functions for a client
   connect -- :: (DescribesHost host,DescribesPort port) => 
           --    host -> port -> IO HandleEV
   -- connect attempts to connect to the server
   ) where

import Socket
import Channels
import EV
import FileEV
import Thread
import Debug(debug)

newtype HostDesc = HostDesc String deriving Show

class DescribesHost a where
   makeHost :: a -> IO HostDesc

instance DescribesHost String where
   makeHost name = return (HostDesc name)

instance DescribesHost HostDesc where
   makeHost = return

newtype PortDesc = PortDesc Int deriving Show

descToPort (PortDesc port) = PortNumber(fromIntegral port)   

class DescribesPort a where
   makePort :: a -> IO PortDesc

instance DescribesPort Int where
   makePort name = return (PortDesc name)   

instance DescribesPort PortDesc where
   makePort = return

listenEV :: DescribesPort port => 
   port -> IO(EV(HostDesc,PortDesc,HandleEV))
listenEV portDesc =
   do
      port <- makePort portDesc
      socket <- listenOn(descToPort port)
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
               
connect :: (DescribesHost host,DescribesPort port) =>
   host -> port -> IO HandleEV
connect host port =
   do
      (HostDesc hostName) <- makeHost host
      portDesc <- makePort port
      handle <- connectTo hostName (descToPort portDesc)
      makeFileEV handle


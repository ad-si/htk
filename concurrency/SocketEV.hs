{- SocketEV provides a simple encapsulation of the GHC Socket interface
   (and therefore BSD sockets) in terms of events.  Communication
   is via the tcp interface in line mode.  (So strings should not
   contain newlines.)
   -}
module SocketEV(
   -- general functions
   HostDesc, -- description of the host, instance of Show
   
   getHostString, -- :: DescribesHost host => host -> IO String

   DescribesHost(makeHost), -- :: a -> IO HostDesc
      -- defined for Strings and HostDesc
   PortDesc, -- description of the port, instance of Show
   DescribesPort(makePort), -- :: a -> IO PortDesc
      -- defined for Int and PortDesc

   getPortNumber, -- :: DescribesPort port => port -> IO PortNumber
   -- functions for a server
   listenEV, -- :: DescribesPort port => port 
             --      -> IO(EV(HostDesc,PortDesc,Handle))
   -- listenEV should only be called once per port.  It returns
   -- an event which happens when new clients attempt to call,
   -- giving the clients host, port and a Handle for accessing it.

   -- functions for a client
   connect -- :: (DescribesHost host,DescribesPort port) => 
           --    host -> port -> IO Handle
   -- connect attempts to connect to the server
   ) where

import IO

import Socket

import Channels
import EV
import Thread
import Debug(debug,(@:))

newtype HostDesc = HostDesc String deriving Show

class DescribesHost a where
   makeHost :: a -> IO HostDesc

instance DescribesHost String where
   makeHost name = return (HostDesc name)

instance DescribesHost HostDesc where
   makeHost = return

getHostString :: DescribesHost host => host -> IO String
getHostString hostDesc =
   do
      HostDesc name <- makeHost hostDesc
      return name

newtype PortDesc = PortDesc Int deriving Show

class DescribesPort a where
   makePort :: a -> IO PortDesc

instance DescribesPort Int where
   makePort name = return (PortDesc name)   

instance DescribesPort PortDesc where
   makePort = return

getPortNumber :: DescribesPort port => port -> IO PortID
getPortNumber portDesc =
   do
      PortDesc portNo <- makePort portDesc
      return (PortNumber(fromIntegral portNo))

listenEV :: DescribesPort port => port -> IO(EV(HostDesc,PortDesc,Handle))
listenEV portDesc =
   do
      portNumber <- getPortNumber portDesc
      socket <- listenOn portNumber
      newConnections <- 
         newChannel :: (IO (Channel (HostDesc,PortDesc,Handle)))
      let
         listenerThread = 
            do
               (handle,hostName,portNumber) <- accept socket
               let
                  hostDesc = HostDesc hostName
                  portDesc = PortDesc (fromIntegral portNumber)
               sendIO newConnections (hostDesc,portDesc,handle)
               listenerThread
      forkIO listenerThread
      return (receive newConnections) 
               
connect :: (DescribesHost host,DescribesPort port) => 
   host -> port -> IO Handle
connect hostDesc portDesc =
   do
      hostName <- getHostString hostDesc
      portNumber <- getPortNumber portDesc
      connectTo hostName portNumber


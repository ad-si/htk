{- HostPorts provides an abstract interface for describing hosts and
   ports. -}
module HostsPorts(
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

   -- connect to a socket
   connect, -- :: (DescribesHost host,DescribesPort port) => 
   -- host -> port -> IO Handle

   ) where

import IO
import Socket


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

connect :: (DescribesHost host,DescribesPort port) => 
   host -> port -> IO Handle
connect hostDesc portDesc =
   do
      hostName <- getHostString hostDesc
      portNumber <- getPortNumber portDesc
      connectTo hostName portNumber

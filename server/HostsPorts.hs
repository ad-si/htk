{- HostPorts provides an abstract interface for describing hosts and
   ports. -}
module HostsPorts(
   HostPort, -- description of a host and port

   DescribesHost, -- class.  Instanced for Strings
   DescribesPort, -- class.  Instanced for Ints

   mkHostPort, 
      -- :: (DescribesHost host,DescribesPort port)
      -- => host -> port -> IO HostPort

   connect, -- :: (? server :: HostPort) => IO Handle

   getPortNumber, -- :: DescribesPort port => port -> IO PortNumber

   getDefaultHostPort, -- :: IO HostPort
      -- extract HostPort from WBFiles settings.
   ) where

import IO
import Socket

import Control.Exception

import Debug
import WBFiles
import Thread

-- -------------------------------------------------------------------
-- The datatypes
-- -------------------------------------------------------------------

data HostPort = HostPort {
   host :: String,
   port :: PortID
   }

newtype HostDesc = HostDesc String deriving Show


newtype PortDesc = PortDesc Int deriving Show

-- -------------------------------------------------------------------
-- The classes and instances
-- -------------------------------------------------------------------

class DescribesHost a where
   makeHost :: a -> IO HostDesc

instance DescribesHost String where
   makeHost name = return (HostDesc name)

instance DescribesHost HostDesc where
   makeHost = return

class DescribesPort a where
   makePort :: a -> IO PortDesc

instance DescribesPort Int where
   makePort name = return (PortDesc name)   

instance DescribesPort PortDesc where
   makePort = return

-- -------------------------------------------------------------------
-- The functions
-- -------------------------------------------------------------------

mkHostPort :: (DescribesHost host,DescribesPort port) 
   => host -> port -> IO HostPort
mkHostPort host port =
   do
      (HostDesc hostStr) <- makeHost host
      portID <- getPortNumber port
      return (HostPort {host = hostStr,port = portID})


getHostString :: DescribesHost host => host -> IO String
getHostString hostDesc =
   do
      HostDesc name <- makeHost hostDesc
      return name

getPortNumber :: DescribesPort port => port -> IO PortID
getPortNumber portDesc =
   do
      PortDesc portNo <- makePort portDesc
      return (PortNumber(fromIntegral portNo))

connect :: (?server :: HostPort) => IO Handle
connect = repeatConnectTo (host ?server) (port ?server)

repeatConnectTo :: HostName -> PortID -> IO Handle
repeatConnectTo = innerConnect True
   where
      innerConnect :: Bool -> HostName -> PortID -> IO Handle
      innerConnect firstTime hostName portNumber =
         do
            result <- Control.Exception.try (connectTo hostName portNumber)
            case result of
               Right handle -> 
                  do
                     if firstTime
                        then
                           done
                        else
                           putStrLn "Attempt to connect to server succeeded!" 
                     return handle
               Left excep ->
                  do
                     putStrLn ("Attempt to connect to server failed: "
                        ++ show excep
                        ++ "\n Retrying in 0.5 seconds")
                     delay (secs 0.5)
                     innerConnect False hostName portNumber


getDefaultHostPort :: IO HostPort
getDefaultHostPort =
   do
      hostOpt <- getServer
      port <- getPort
      host <- case hostOpt of
         Nothing -> error "server not specified!"
         Just host -> return host
      mkHostPort host port
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

   -- write and then flush 
   hPutStrLnFlush, -- :: Handle -> String -> IO ()

   ) where

import IO
import Socket

import Control.Exception

import Debug
import Thread

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
      debug portNo
      return (PortNumber(fromIntegral portNo))

connect :: (DescribesHost host,DescribesPort port) => 
   host -> port -> IO Handle
connect hostDesc portDesc =
   do
      hostName <- getHostString hostDesc
      debug hostName
      portNumber <- getPortNumber portDesc
      handle <- repeatConnectTo hostName portNumber
--      hSetBuffering handle NoBuffering
      return handle

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
 

hPutStrLnFlush :: Handle -> String -> IO ()
hPutStrLnFlush handle string =
   do
      hPutStrLn handle string
      hFlush handle
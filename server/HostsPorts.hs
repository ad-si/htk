{- HostPorts provides an abstract interface for describing hosts and
   ports. -}
module HostsPorts(
   HostPort(..), 
      -- description of a host and port
      -- instance of Eq,Ord,Show.

   DescribesHost, -- class.  Instanced for Strings
   DescribesPort, -- class.  Instanced for Ints

   mkHostPort, 
      -- :: (DescribesHost host,DescribesPort port)
      -- => host -> port -> IO HostPort

   mkHostDescription,
      -- :: String -> Int -> String
      -- Function for getting the textual description of a host, if none
      -- is already known.

   connect, -- :: (? server :: HostPort) => IO Handle

   getPortNumber, -- :: DescribesPort port => port -> IO PortNumber

   getDefaultHostPort, -- :: IO HostPort
      -- extract HostPort from WBFiles settings.

   hostPortForm, -- :: Maybe String -> Maybe Int -> Form HostPort
      -- Form for entering a host and port.

   HostKey, -- instance of Eq, Ord.
   mapHostPort, -- :: HostPort -> HostKey
   ) where

import IO
import Network

import Control.Exception

import Debug
import WBFiles
import Thread
import ExtendedPrelude
import Computation

import SimpleForm

-- -------------------------------------------------------------------
-- The datatypes
-- -------------------------------------------------------------------

data HostPort = HostPort {
   host :: String,
   port :: PortNumber,
   description :: String
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

type HostKey = (String,PortNumber) -- must be an instance of Eq,Ord.

mapHostPort :: HostPort -> HostKey
mapHostPort (HostPort {host = host,port = port}) =
   (host,port)

instance Eq HostPort where
   (==) = mapEq mapHostPort

instance Ord HostPort where
   compare = mapOrd mapHostPort

instance Show HostPort where
   showsPrec n hostPort acc = description hostPort ++ acc

-- -------------------------------------------------------------------
-- The functions
-- -------------------------------------------------------------------

mkHostPort :: (DescribesHost host,DescribesPort port) 
   => host -> port -> IO HostPort
mkHostPort host port =
   do
      (HostDesc hostStr) <- makeHost host
      portNumber  <- getPortNumber port
      let
         description = mkHostDescription hostStr portNumber
      return (HostPort {
         host = hostStr,port = portNumber,description = description})

mkHostDescription :: (Show portNo,Num portNo) => String -> portNo -> String
mkHostDescription hostStr i =
   if i == 11393 
      then
         hostStr
      else
         hostStr ++ ":" ++ show i


getHostString :: DescribesHost host => host -> IO String
getHostString hostDesc =
   do
      HostDesc name <- makeHost hostDesc
      return name

getPortNumber :: DescribesPort port => port -> IO PortNumber
getPortNumber portDesc =
   do
      PortDesc portNo <- makePort portDesc
      return (fromIntegral portNo)

connect :: (?server :: HostPort) => IO Handle
connect = connectTo (host ?server) (PortNumber (port ?server))

getDefaultHostPort :: IO HostPort
getDefaultHostPort =
   do
      hostOpt <- getServer
      port <- getPort
      host <- case hostOpt of
         Nothing -> error "server not specified!"
         Just host -> return host
      mkHostPort host port

hostPortForm :: Maybe String -> Maybe Int -> Form HostPort
hostPortForm serverOpt portOpt =
   let
      hostForm1 :: Form (Maybe String)
      hostForm1 = newFormEntry "Host" serverOpt

      hostForm :: Form String
      hostForm = mapForm
         (\ serverOpt -> case serverOpt of
            Nothing -> hasError "Host must be specified"
            Just server -> hasValue server
            )
         hostForm1

      portForm1 :: Form (Maybe Int)
      portForm1 = newFormEntry "Port" portOpt

      portForm :: Form Int
      portForm = mapForm
         (\ portOpt -> case portOpt of
            Nothing -> hasError "Port must be specified"
            Just port -> hasValue port
            )
         portForm1
 
      form1 = hostForm \\ portForm

      form = mapFormIO
         (\ (host,port) ->
            do
               hostPort <- mkHostPort host port
               return (hasValue hostPort)
            )
         form1
   in
      form
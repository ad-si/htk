{- HostPorts provides an abstract interface for describing hosts and
   ports. -}
module HostsPorts(
   HostPort, 
      -- description of a host and port
      -- instance of Eq,Ord,Show.

   DescribesHost, -- class.  Instanced for Strings
   DescribesPort, -- class.  Instanced for Ints

   mkHostPort, 
      -- :: (DescribesHost host,DescribesPort port)
      -- => host -> port -> IO HostPort

   connect, -- :: (? server :: HostPort) => IO Handle

   getPortNumber, -- :: DescribesPort port => port -> IO PortNumber

   getDefaultHostPort, -- :: IO HostPort
      -- extract HostPort from WBFiles settings.

   hostPortForm, -- :: Maybe String -> Maybe Int -> Form HostPort
      -- Form for entering a host and port.

   ) where

import IO
import Socket

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

mapHostPort :: HostPort -> (String,PortNumber)
mapHostPort (HostPort {host = host,port = PortNumber port}) =
   (host,port)
   -- we may have to handle the other constructors one day, but not yet,
   -- since there is no way of constructing a HostPort with them.

instance Eq HostPort where
   (==) = mapEq mapHostPort

instance Ord HostPort where
   compare = mapOrd mapHostPort

instance Show HostPort where
   showsPrec n hostPort acc =
      let
         (str,i) = mapHostPort hostPort
      in    
         (str ++) . (':' :) . (showsPrec n i) $ acc

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
connect = connectTo (host ?server) (port ?server)

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
-- | HostPorts provides an abstract interface for describing hosts and
-- ports. 
module HostsPorts(
   HostPort,host,port,description, 
      -- description of a host and port, and some of its fields.
      -- instance of Eq,Ord,Show.
   LoginInfo(..),
      -- user and password.

   toLoginInfo, 
      -- :: HostPort -> Maybe LoginInfo
      -- Extract the LoginInfo for a HostPort, if any were supplied.

   DescribesHost, -- class.  Instanced for Strings
   DescribesPort, -- class.  Instanced for Ints

   mkHostDescription,
      -- :: String -> Int -> String
      -- Function for getting the textual description of a host, if none
      -- is already known.

   fromHostDescription,
      -- :: String -> IO (WithError HostPort)
      -- Turn the textual description of a host into a HostPort,
      -- reversing mkHostDescription.
   fromHostDescription1,
      -- :: String -> LoginInfo -> IO (WithError HostPort)
      -- Turn the textual description of a host into a HostPort, and also
      -- include the login data in the HostPort.
   setDescription,
      -- :: String -> HostPort -> HostPort
      -- Change the textual description of a HostPort.
   mkHostPort,
      -- :: (DescribesHost host,DescribesPort port) 
      -- => host -> port -> Maybe String -> Maybe LoginInfo -> IO HostPort
      -- The general function for constructing HostPort.

   connect, -- :: (? server :: HostPort) => IO Handle

   getPortNumber, -- :: DescribesPort port => port -> IO PortNumber

   getDefaultHostPort, -- :: IO HostPort
      -- extract HostPort from WBFiles settings.

   hostPortForm, -- :: Maybe String -> Maybe Int -> Form HostPort
      -- Form for entering a host and port.
      -- (This does not prompt for login information)

   HostKey, -- instance of Eq, Ord.
   mapHostPort, -- :: HostPort -> HostKey
   ) where

import Maybe
import IO
import Network

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
   description :: String,
   loginInfo :: Maybe LoginInfo
   }

newtype HostDesc = HostDesc String deriving Show

newtype PortDesc = PortDesc Int deriving Show

data LoginInfo = LoginInfo {
   user :: String,
   password :: String
   }

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
   => host -> port -> Maybe String -> Maybe LoginInfo -> IO HostPort
mkHostPort host port descriptionOpt loginInfo  =
   do
      (HostDesc hostStr) <- makeHost host
      portNumber  <- getPortNumber port
      let
         description =
            fromMaybe (mkHostDescription hostStr portNumber) descriptionOpt
      return (HostPort {
         host = hostStr,
         port = portNumber,
         description = description,
         loginInfo = loginInfo
         })   

mkHostDescription :: (Show portNo,Num portNo) => String -> portNo -> String
mkHostDescription hostStr i =
   if i == defaultPort
      then
         hostStr
      else
         hostStr ++ ":" ++ show i

 
-- | Turn the textual description of a host into a HostPort,
-- reversing mkHostDescription.
fromHostDescription :: String -> IO (WithError HostPort)
fromHostDescription description = fromHostDescription' description Nothing

-- | Turn the textual description of a host into a HostPort, and also
-- include the login data in the HostPort.
fromHostDescription1 :: String -> LoginInfo -> IO (WithError HostPort)
fromHostDescription1 str loginInfo = fromHostDescription' str (Just loginInfo)

fromHostDescription' :: String -> Maybe LoginInfo -> IO (WithError HostPort)
fromHostDescription' description loginInfoOpt =
   do
      let
          (hostPortWE :: WithError (String,Int)) 
             = case splitToChar ':' description of
                Nothing -> hasValue (description,defaultPort)
                Just (host,portStr) 
                   | (Just port) <- readCheck portStr
                      -> hasValue (host,port)
                   | True -> hasError ("Cannot parse port number " ++ portStr
                      ++ " in " ++ description)
     
      mapWithErrorIO
         (\ (host,port) -> mkHostPort host port Nothing loginInfoOpt)
         hostPortWE

-- | Change the textual description of a HostPort.
setDescription :: String -> HostPort -> HostPort
setDescription description1 hostPort0 =
   hostPort0 {description = description1}

-- | Extract the LoginInfo for a HostPort, if any were supplied.
toLoginInfo :: HostPort -> Maybe LoginInfo
toLoginInfo = loginInfo

defaultPort :: Num portNo => portNo
defaultPort = 11393

{- unused
getHostString :: DescribesHost host => host -> IO String
getHostString hostDesc =
   do
      HostDesc name <- makeHost hostDesc
      return name
-}

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
      mkHostPort host port Nothing Nothing

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
               hostPort <- mkHostPort host port Nothing Nothing
               return (hasValue hostPort)
            )
         form1
   in
      form
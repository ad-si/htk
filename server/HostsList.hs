{- This module maintains the list of hosts we know about. -}
module HostsList (
   getHostPorts, 
      -- :: IO [HostPort]
      -- get a list of all host-ports we currently know about.

   getUserPassword, 
      -- :: Bool -> HostPort -> IO (Maybe (String,String))
      -- getUserPassword returns a user and password for the given hostPort,
      -- querying the user if necessary.
      -- If the Bool is set we query the user anyway.
      --
      -- Nothing is returned if the user cancels.

   defaultUser,
      -- :: String
      -- The default userId (used for the internal server).

   ) where

import Maybe

import Data.IORef
import System.IO.Unsafe

import Socket

import Computation
import WBFiles
import ExtendedPrelude

import Registry

import SimpleForm

import Text.XML.HaXml.Xml2Haskell

import DTD_Hosts
import HostsPorts

-- --------------------------------------------------------------------------
-- The default user.  This is suggested as a default for servers when no
-- user-id is given in the password file, and is also used for the internal
-- server.
-- --------------------------------------------------------------------------

defaultUser :: String
defaultUser = unsafePerformIO getDefaultUser
{-# NOINLINE defaultUser #-}

getDefaultUser :: IO String
getDefaultUser =
   do
      userOpt <- getUser -- can be set by an option/environment variable.
      case userOpt of
         Just user -> return user
         Nothing ->
            doFormMust "Default User" (userForm "")
                                 

userForm :: String -> Form String
userForm defUser =
   let
      userForm1 :: Form String
      userForm1 = newFormEntry "User Id" defUser

      userForm2 :: Form String
      userForm2 = fmap trimSpaces userForm1

      userForm3 :: Form String
      userForm3 = guardForm (/= "") 
         "User Id must be non-empty" userForm2
   in
      userForm3

-- --------------------------------------------------------------------------
-- Managing the various hosts.
-- --------------------------------------------------------------------------

getHostPorts :: IO [HostPort]
getHostPorts =
   do
      contents <- listRegistryContents hostsRegistry
      return (map (thisHostPort . snd) contents)

-- | getUserPassword returns a user and password for the given hostPort,
-- querying the user if necessary.
-- If the Bool is set we query the user anyway.
--
-- Nothing is returned if the user cancels.
getUserPassword :: Bool -> HostPort -> IO (Maybe (String,String))
getUserPassword mustQuery hostPort =
   do
      userPassword <- transformValue hostsRegistry (mapHostPort hostPort)
         (\ hostDataOpt ->
            case hostDataOpt of
               Just hostData -> return (hostDataOpt,userPassword hostData)
               Nothing ->
                  do
                     userPassword <- newIORef (Nothing,Nothing)
                     let 
                        hostData = HostData {thisHostPort = hostPort,
                           userPassword = userPassword}
                     return (Just hostData,userPassword)
            )

      (userOpt,passwordOpt) <- readIORef userPassword
      case (userOpt,passwordOpt) of
         (Just user,Just password) | (not mustQuery)
            -> return (Just (user,password))
         _ ->
            do
               let
                  userDefault = case userOpt of
                     Nothing -> defaultUser
                     Just user -> user

                  passwordDefault = fromMaybe "" passwordOpt 
               seq userDefault done
                  -- force default-user window to appear, if necessary.
               let
                  form1 :: Form String
                  form1 = userForm userDefault

                  form2a :: Form (Password (Maybe String))
                  form2a = newFormEntry "Password" 
                     (Password (Just passwordDefault))

                  form2 :: Form String
                  form2 = mapForm
                     (\ (Password passwordOpt) -> case passwordOpt of
                        Just password -> hasValue password
                        Nothing -> hasError "Password must be specified"
                        )
                     form2a

               userPasswordOpt <- doForm "Connecting to Server" 
                  (form1 // form2)

               case userPasswordOpt of
                  Nothing -> done
                  Just (user,password) ->
                     do
                        when ((user /= userDefault) 
                              || (password /= passwordDefault))
                           (writeIORef userPassword (Just user,Just password))

               return userPasswordOpt

data HostData = HostData {
   thisHostPort :: HostPort,
   userPassword :: IORef (Maybe String,Maybe String)
      -- username and password for this server.
   }

hostsRegistry :: Registry HostKey HostData
hostsRegistry = unsafePerformIO getHostsRegistry
{-# NOINLINE hostsRegistry #-}

getHostsRegistry :: IO (Registry HostKey HostData)
getHostsRegistry =
   do
      hosts <- getHosts
      (Hosts hostList) <- readXml hosts
      let
         err s = error ("In " ++ hosts ++ ": " ++ s)

         mapOne :: Host -> IO (HostKey,HostData)
         mapOne host =
            do
               let
                  host1 = hostHostName host

                  portStr = case hostPort host of
                     Default portStr -> portStr
                     NonDefault portStr -> portStr
                  
                  (portNum :: Int) = case readCheck portStr of
                     Just portNum -> portNum
                     Nothing -> err (show portStr 
                        ++ " is not a valid port number")

               port1 <- getPortNumber portNum

               let
                  description1 = fromMaybe
                     (mkHostDescription host1 portNum)
                     (hostDescription host)

                  hostPort1 = HostPort {
                     host = host1,port = port1,description = description1}
               userPassword1 <- newIORef (hostUser host,Nothing)
               let
                  hostData = HostData {
                     thisHostPort = hostPort1,
                     userPassword = userPassword1
                     }
               return (mapHostPort hostPort1,hostData)

      registryContents <- mapM mapOne hostList
      listToNewRegistry registryContents
-- | This module maintains the list of hosts we know about.
module Server.HostsList (
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

import Util.Computation
import Util.WBFiles
import Util.ExtendedPrelude
import Util.Messages

import Util.Registry

import Posixutil.CopyFile

import HTk.Toolkit.SimpleForm

import Text.XML.HaXml.Xml2Haskell

import Server.Hosts
import Server.HostsPorts

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

      htk <- htkPresent
      case userOpt of
         Just user -> return user
         Nothing ->
            if htk
               then
                  doFormMust "Default User" (userForm "")
               else
                  let
                     du =
                        do
                           user1 <- textQuery "Default User"
                           if user1 == ""
                              then
                                 do
                                    putStrLn "User Id must be non-empty"
                                    du
                              else
                                 return user1
                  in
                     du

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

               htk <- htkPresent
               userPasswordOpt <-
                  if htk
                     then
                        doForm "Connecting to Server" (form1 // form2)
                     else
                        do
                           user1 <- textQuery ("Enter user id, or just hit"
                              ++ " RETURN for " ++ userDefault)
                           let
                              user =
                                 if user1 == "" then userDefault else user1
                           password1 <- textQuery ("Enter password, or hit"
                              ++ " RETURN to cancel operation")
                           if password1 == ""
                              then
                                 return Nothing
                              else
                                 return (Just (user,password1))

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
      hostsString <- copyFileToString hosts
      let
         Just (Hosts hostList) = readXml hostsString
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

                  (port1 :: Int) = case readCheck portStr of
                     Just portNum -> portNum
                     Nothing -> err (show portStr
                        ++ " is not a valid port number")

               hostPort1 <- mkHostPort host1 port1 (hostDescription host)
                  Nothing

               userPassword1 <- newIORef (hostUser host,Nothing)
               let
                  hostData = HostData {
                     thisHostPort = hostPort1,
                     userPassword = userPassword1
                     }
               return (mapHostPort hostPort1,hostData)

      registryContents <- mapM mapOne hostList
      listToNewRegistry registryContents

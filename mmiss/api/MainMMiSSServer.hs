{- This module contains the main function for the MMiSS server, and does the
   business of accepting sockets, authentication and so on.

   Some code has been poached from server/Server.hs.
   -}
module Main(main) where

import IO
import Time

import Control.Exception
import Control.Concurrent
import System.Posix.Signals
import Network


import Computation
import ExtendedPrelude
import BinaryAll
import IOExtras
import WBFiles(getXMLPort)

import Crypt
import PasswordFile
import HostsPorts

import Registrations

import MMiSSRegistrations

import MMiSSDoXml

main :: IO ()
main =
   do
      installHandler sigPIPE Ignore Nothing

      doRegistrations
      doMMiSSRegistrations

      portDesc <- getXMLPort
      portNumber <- getPortNumber portDesc
      socket <- listenOn (PortNumber portNumber)
      let
         serverAction =
            do
               (handle,hostName,_) <- accept socket

               hSetBuffering handle (BlockBuffering (Just 4096))
               forkIO (mainHandle handle hostName)
               serverAction
 
      serverAction

mainHandle :: Handle -> String -> IO ()
mainHandle handle hostName  =
   do
      userWE <- addFallOutWE (\ break -> breakOtherExceps break (
         do
            serviceWE <- readString handle
            service <- coerceWithErrorOrBreakIO break serviceWE

            userIdWE <- readString handle
            userId <- coerceWithErrorOrBreakIO break userIdWE

            passwordWE <- readString handle
            password <- coerceWithErrorOrBreakIO break passwordWE

            if (service /= "MMiSS-XML")
               then
                  break ("Service: " ++ service ++ " not recognised")
               else
                  done

            let
               authError = break "Unable to authenticate user"

            userOpt <- getUserEntry userId
            user <- case userOpt of
               Nothing -> authError
               Just user -> return user

            passwordOK <- verifyPassword password 
               (encryptedPassword user)
            if passwordOK 
               then
                  return user
               else
                  authError
         ))

      Control.Exception.try ( 
         -- general wrapper to catch IO errors
         case fromWithError userWE of
            Right user ->
               do
                  writeString handle "OK"

                  clockTime <- getClockTime
                  calendarTime <- toCalendarTime clockTime

                  putStrLn (userId user ++ "@" ++ hostName ++ ":"
                     ++ calendarTimeToString calendarTime) 
                  hFlush stdout
                  hFlush handle         

                  doXml handle user
            Left mess ->
               do
                  putStrLn (hostName ++ ": Connection failed")
                  hFlush stdout
                  writeString handle ("ERROR: " ++ mess)
                  hClose handle
         )

      done

-- ----------------------------------------------------------------------------
-- Functions for reading and writing Strings during login.
-- ----------------------------------------------------------------------------


readString :: Handle -> IO (WithError String)
readString = hReadLtd (maxLen + 1)

writeString :: Handle -> String -> IO ()
writeString handle str = hWrite handle str

maxLen :: Int
maxLen = 127
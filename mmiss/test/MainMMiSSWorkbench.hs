{- Runs the MMiSS workbench as a client. -}
module Main(main) where

import Maybe
import System

import Posix hiding (Quit)

import Debug(debug)
import WBFiles
import Computation
import ExtendedPrelude

import Events
import Destructible
import InfoBus

import HTk
import SimpleForm
import DialogWin

import DaVinciGraph

import HostsPorts

import VersionGraph
import VersionGraphList
import Registrations

import EmacsBasic

import MMiSSRegistrations

main =
   do
      parseArgumentsRequiring [
         "top",
         "server",
         "editor"
         ]

      emacsWorkingWE <- isEmacsWorking
      coerceWithErrorIO emacsWorkingWE

      withdrawWish
      seq loadHTkImages done 

      doRegistrations
      doMMiSSRegistrations

      mainWindow


data UserAction =
      Connect HostPort
   |  Quit -- doesn't seem to work right now.

mainWindow :: IO ()
mainWindow =
   do
      serverOpt <- getServer
     
      (remoteServer :: Maybe (Form HostPort,String)) <- case serverOpt of
         Nothing -> return Nothing
         Just server ->
            do
               hostPort <- getDefaultHostPort
               let
                  serverName = case splitToChar '.' server of
                     Nothing -> server
                     Just (serverName,_) -> serverName

                  remoteForm = fmap
                     (\ () -> hostPort)
                     (nullForm serverName)
               return (Just (remoteForm,"Connect"))

      (localServer :: Maybe (Form HostPort,String)) <-
         do
            hostPort <- mkHostPort "localhost" (11393 :: Int)
            let
               localForm = fmap
                  (\ () -> hostPort)
                  (nullForm "Local Server")
            return (Just (localForm,"Connect"))

      let
         (otherServer :: Maybe (Form HostPort,String)) =
            Just (hostPortForm Nothing Nothing,"Connect")

         serverForms :: [(Form HostPort,String)]
         serverForms = catMaybes [remoteServer,localServer,otherServer]

         map' :: (a -> b) -> (Form a,String) -> (Form b,String)
         map' fn (form,s) = (fmap fn form,s)
  
         quitForm :: (Form (),String)
         quitForm = (nullForm "","Quit")

         mainFormList =
            (map (map' Connect) serverForms) ++ [map' (const Quit) quitForm]

      (event,closeWindow) <- doFormList "MMiSS action" mainFormList 
      let
         mainLoop :: IO ()
         mainLoop =
            do
               actionWE <- sync event
               case fromWithError actionWE of
                  Left mess ->
                     do
                        createErrorWin mess []
                        mainLoop
                  Right action -> doAction action               

         doAction :: UserAction -> IO ()
         doAction (Connect hostPort) =
            do
               addVersionGraph daVinciSort hostPort
               mainLoop

         doAction Quit =
            do
               reallyQuit <- createConfirmWin
                  "Exit Workbench without saving anything?"
                  []
               if reallyQuit
                  then
                     do
                        closeWindow
                        versionGraphList <- getCurrentVersionGraphs
                        mapM 
                           (\ (_,versionGraph) -> destroy versionGraph)
                           versionGraphList
                        cleanupWish
                        exitImmediately ExitSuccess
                  else
                     mainLoop

      mainLoop 
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
import HostsList

import VersionGraph
import VersionGraphList
import Registrations

import EmacsBasic

import MMiSSRegistrations

-- We put a catchOurExceps round everything, just in case.
main =
   do
      ourExcep <- catchOurExceps main1
      case ourExcep of
         Left mess -> putStrLn mess
         Right () -> done

main1 =
   do
      parseArgumentsRequiring [
         "top",
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
   |  ConnectInternal
   |  Quit -- only partially works.

mainWindow :: IO ()
mainWindow =
   do
      hostPorts <- getHostPorts
      let
         remoteServers :: [(Form HostPort,String)]
         remoteServers = 
            map
               (\ hostPort ->
                  let
                     form = fmap
                        (\ () -> hostPort)
                        (nullForm (description hostPort))
                  in
                     (form,"Connect")
                  )
               hostPorts

         otherServer :: (Form HostPort,String)
         otherServer = (hostPortForm Nothing Nothing,"Connect")

         serverForms :: [(Form HostPort,String)]
         serverForms = remoteServers ++ [otherServer]

         map' :: (a -> b) -> (Form a,String) -> (Form b,String)
         map' fn (form,s) = (fmap fn form,s)

         internalForm :: (Form (),String)
         internalForm = (nullForm "Local Working Area","Connect")
  
         quitForm :: (Form (),String)
         quitForm = (nullForm "","Quit")

         mainFormList =
            (map (map' Connect) serverForms) 
               ++ [map' (const ConnectInternal) internalForm,
                  map' (const Quit) quitForm]

      (event,closeWindow) <- doFormList "MMiSS Repository Client Control Center" mainFormList 
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
               addVersionGraph daVinciSort (Just hostPort)
               mainLoop
         doAction ConnectInternal =
            do
               addVersionGraph daVinciSort Nothing
               mainLoop
         doAction Quit =
            do
               reallyQuit <- createConfirmWin
                  "Do you really want to quit?"
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
                        shutdown
                        exitImmediately ExitSuccess
                  else
                     mainLoop

      mainLoop 

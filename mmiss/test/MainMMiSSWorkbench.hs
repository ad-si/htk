{- Runs the MMiSS workbench as a client. -}
module Main(main) where

import Maybe
import System

import Util.WBFiles
import Util.ExtendedPrelude
import Util.Messages

import Reactor.InfoBus

import HTk.Toplevel.HTk
import HTk.Toolkit.SimpleForm
import HTk.Toolkit.DialogWin

import UDrawGraph.Graph

import Server.PasswordFile
import Server.HostsPorts
import Server.HostsList

import Types.VersionGraphList
import Types.Registrations

import Emacs.Basic

import MMiSS.Registrations

-- We put a catchOurExceps round everything, just in case.
main :: IO ()
main =
   do
      ourExcep <- catchOurExceps main1
      case ourExcep of
         Left mess -> putStrLn mess
         Right () -> done

main1 :: IO ()
main1 =
   do
      parseArgumentsRequiring [
         "top",
         "editor",
         "daVinci",
         "gnuclient",
         "toolTimeOut"
         ]

      emacsWorkingWE <- isEmacsWorking
      coerceWithErrorIO emacsWorkingWE

      withdrawWish
      seq loadHTkImages done

      alwaysAllowAdmin
         -- allow admin status to be claimed by the internal server
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
      useHTk
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
                        errorMess mess
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
               reallyQuit <- confirmMess
                  "Do you really want to quit?"
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
                        exitWith ExitSuccess
                  else
                     mainLoop

      mainLoop

{- Runs the MMiSS workbench as a client. -}
module Main(main) where

import System

import Posix

import Debug(debug)
import WBFiles
import Computation

import Events
import Destructible
import InfoBus

import HTk
import DialogWin

import DaVinciGraph

import HostsPorts

import VersionGraph

import EmacsBasic

import MMiSSInitialise

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

      server <- getDefaultHostPort

      repository <- 
         let
            ?server = server
         in
            mmissInitialise

      versionGraph <- newVersionGraph daVinciSort repository
      sync (destroyed versionGraph)
      cleanupWish
      exitImmediately ExitSuccess


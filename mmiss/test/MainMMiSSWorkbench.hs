{- Runs the MMiSS workbench as a client. -}
module Main(main) where

import System

import Posix

import Debug(debug)
import WBFiles

import Events
import Destructible

import HTk

import DaVinciGraph

import VersionGraph

import MMiSSInitialise

main =
   do
      parseArgumentsRequiring [
         "top",
         "server"
         ]
      withdrawWish
      repository <- mmissInitialise
      versionGraph <- newVersionGraph daVinciSort repository
      sync (destroyed versionGraph)
      cleanupWish
      exitImmediately ExitSuccess


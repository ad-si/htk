{- This program runs a server for all the UniForM services. -}
module Main(main) where

import IO

import Server

import EchoService
import AllocateService
import GraphEditorService
import VersionGraphService

main = runServer [
   echoServiceWrapped,
   allocateServiceWrapped,
   graphEditorServiceWrapped,
   versionGraphServiceWrapped
   ]

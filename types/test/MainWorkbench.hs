{- Runs the workbench as a client.  The following need to be running:
   (1) a CVS repository, with location indicated in UNICVSROOT
   (2) the UniForM server, with hostname indicated in UNISERVER.
   (3) a string describing how to call the editor, see uni/types/CallEditor.

   The script runWorkbench in this directory, provided as an example, sets
   it all going on the same machine.
   -}
module Main(main) where

import System

import Util.WBFiles

import Events.Events
import Events.Destructible

import HTk.Toplevel.HTk

import Server.HostsPorts

import UDrawGraph.Graph

import Types.Initialisation
import Types.VersionGraph

main :: IO ()
main =
   do
      parseArgumentsRequiring [
         "editor",
         "top",
         "server"
         ]
      withdrawWish
      hostPort <- getDefaultHostPort
      versionGraph <-
         let
            ?server = hostPort
         in
            do
               repository <- initialise
               newVersionGraph daVinciSort repository
      sync (destroyed versionGraph)
      cleanupWish
      exitWith ExitSuccess

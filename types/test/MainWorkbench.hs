{- Runs the workbench as a client.  The following need to be running:
   (1) a CVS repository, with location indicated in UNICVSROOT
   (2) the UniForM server, with hostname indicated in UNISERVER.
   (3) a string describing how to call the editor, see uni/types/CallEditor.

   The script runWorkbench in this directory, provided as an example, sets 
   it all going on the same machine.
   -}
module Main(main) where

import Debug(debug)
import WBFiles

import Events
import Destructible

import HTk

import DaVinciGraph

import Initialisation
import VersionGraph

main =
   do
      parseArgumentsRequiring [
         "editor",
         "cvsRoot",
         "top",
         "server"
         ]
      repository <- initialise
      versionGraph <- newVersionGraph daVinciSort repository
      sync (destroyed versionGraph)
      finishHTk      
{- This runs a graph editor attached to a server.  The server's
   name is expected to be the (only) argument. -}
module Main(main) where

import System

import WBFiles

import InfoBus(shutdown)

import HostsPorts

import GraphEditorRemote
import DaVinciGraph

main =
   do
      parseArgumentsRequiring ["wish","daVinci","server","port"]
      server <- getDefaultHostPort
      let ?server = server in graphEditorRemote daVinciSort
      shutdown

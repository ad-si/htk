{- This runs a graph editor attached to a server.  The server's
   name is expected to be the (only) argument. -}
module Main(main) where

import System

import WBFiles

import InfoBus(shutdown)

import GraphEditorRemote
import DaVinciGraphDisp

main =
   do
      parseArgumentsRequiring ["wish","daVinci","server","port"]
      graphEditorRemote daVinciSort
      shutdown

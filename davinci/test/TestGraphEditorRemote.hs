{- This runs a graph editor attached to a server.  The server's
   name is expected to be the (only) argument. -}
module Main(main) where

import System

import GraphEditorRemote
import DaVinciGraphDisp

main =
   do
      [server] <- getArgs
      graphEditorRemote daVinciSort server
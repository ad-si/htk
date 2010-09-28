{- This runs a graph editor attached to a server.  The server's
   name is expected to be the (only) argument. -}
module Main(main) where

import Util.WBFiles

import Reactor.InfoBus(shutdown)

import Server.HostsPorts

import Graphs.GraphEditorRemote
import UDrawGraph.Graph

main :: IO ()
main =
   do
      parseArgumentsRequiring ["wish","daVinci","server","port"]
      server <- getDefaultHostPort
      let ?server = server in graphEditorRemote daVinciSort
      shutdown

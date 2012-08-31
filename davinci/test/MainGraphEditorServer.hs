{- This is the main program for a GraphEditor server.
   -}
module Main(main) where

import Util.WBFiles

import Server.Server
import Graphs.GraphEditorService

main :: IO ()
main =
   do
      parseArgumentsRequiring ["port"]
      runServer [graphEditorServiceWrapped]

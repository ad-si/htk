{- This is the main program for a GraphEditor server.
   -}
module Main(main) where

import IO

import Server
import GraphEditorService

main = runServer (11393::Int) [graphEditorServiceWrapped]

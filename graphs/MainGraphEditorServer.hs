{- This is the main program for a GraphEditor server.
   -}
module Main(main) where

import IO

import WBFiles

import Server
import GraphEditorService

main = 
   do
      parseArgumentsRequiring ["port"]
      runServer [graphEditorServiceWrapped]

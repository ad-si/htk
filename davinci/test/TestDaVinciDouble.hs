{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import Control.Concurrent

import Util.WBFiles(parseArguments)

import Reactor.InfoBus(shutdown)

import GraphDispTest

import UDrawGraph.Graph

main :: IO ()
main =
   do
      parseArguments
      forkIO (setUpGraph daVinciSort)
      setUpGraph daVinciSort
      shutdown


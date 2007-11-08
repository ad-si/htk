{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import Control.Concurrent

import WBFiles(parseArguments)

import InfoBus(shutdown)

import GraphDispTest

import DaVinciGraph

main :: IO ()
main =
   do
      parseArguments
      forkIO (setUpGraph daVinciSort)
      setUpGraph daVinciSort
      shutdown


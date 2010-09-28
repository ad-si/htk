{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import UDrawGraph.Graph
import Reactor.InfoBus(shutdown)
import Util.WBFiles(parseArguments)

main :: IO ()
main =
   do
      parseArguments
      setUpGraph daVinciSort
      shutdown


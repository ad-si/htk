{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import DaVinciGraph
import InfoBus(shutdown)
import WBFiles(parseArguments)

main :: IO ()
main =
   do
      parseArguments
      setUpGraph daVinciSort
      shutdown


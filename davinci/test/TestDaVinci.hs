{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import DaVinciGraphDisp
import SIM(shutdown)
import WBFiles(parseArguments)

main = 
   do
      parseArguments
      setUpGraph daVinciSort
      shutdown
   
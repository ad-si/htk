{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import DaVinciGraphDisp
import SIM(shutdown)
import Concurrent
import WBFiles(parseArguments)

main = 
   do
      parseArguments
      forkIO (setUpGraph daVinciSort)
      setUpGraph daVinciSort 
      shutdown
   
{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import DaVinciGraphDisp
import SIM(shutdown)
import Concurrent

main = 
   do
      forkIO (setUpGraph daVinciSort)
      setUpGraph daVinciSort 
      shutdown
   
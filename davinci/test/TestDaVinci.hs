{- Runs GraphDispTest using DaVinci -}
module Main(main) where

import GraphDispTest
import DaVinciGraphDisp
import SIM(shutdown)

main = 
   do
      setUpGraph daVinciSort
      shutdown
   
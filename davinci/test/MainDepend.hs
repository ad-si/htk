{- Maindepend calls the DrawDepend routine using daVinci. -}
module Main(main) where

import DrawDepend
import DaVinciGraph

main :: IO ()
main = drawDepend daVinciSort

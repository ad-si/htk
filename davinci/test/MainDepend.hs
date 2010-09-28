{- Maindepend calls the DrawDepend routine using daVinci. -}
module Main(main) where

import DrawDepend
import UDrawGraph.Graph

main :: IO ()
main = drawDepend daVinciSort

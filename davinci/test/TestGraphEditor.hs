{- First test of graph editor -}
module Main(main) where


import GraphEditor
import Graph
import DaVinciGraphDisp
import SimpleGraph
import SIM(destroyed,sync)

main =
   do
      (graph :: Displayable SimpleGraph) <- newEmptyGraph 
      graphEditor <- newGraphEditor daVinciSort graph
      sync (destroyed graphEditor)
{- First test of graph editor -}
module Main(main) where



import GraphEditor
import Graph
import DaVinciGraphDisp
import SimpleGraph
import SIM(destroyed,sync,shutdown)
import WBFiles

main =
   do
      parseArguments
      (graph :: Displayable SimpleGraph) <- newEmptyGraph 
      graphEditor <- newGraphEditor daVinciSort graph
      sync (destroyed graphEditor)
      shutdown
{- First test of graph editor -}
module Main(main) where



import GraphEditor
import Graph
import DaVinciGraph
import SimpleGraph
import Destructible(destroyed)
import InfoBus
import Events
import WBFiles

main =
   do
      parseArguments
      (graph :: Displayable SimpleGraph) <- newEmptyGraph 
      graphEditor <- newGraphEditor daVinciSort graph
      sync (destroyed graphEditor)
      shutdown
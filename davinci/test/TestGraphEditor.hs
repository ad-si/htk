{- First test of graph editor -}
module Main(main) where



import Graphs.GraphEditor
import Graphs.Graph
import UDrawGraph.Graph
import Graphs.SimpleGraph
import Events.Destructible(destroyed)
import Reactor.InfoBus
import Events.Events
import Util.WBFiles

main :: IO ()
main =
   do
      parseArguments
      (graph :: Displayable SimpleGraph) <- newEmptyGraph
      graphEditor <- newGraphEditor daVinciSort graph
      sync (destroyed graphEditor)
      shutdown

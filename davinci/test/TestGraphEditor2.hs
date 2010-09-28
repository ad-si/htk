{- Second test of graph editor -}
module Main(main) where


import Graphs.GraphEditor
import Graphs.Graph
import UDrawGraph.Graph
import Graphs.SimpleGraph
import Reactor.InfoBus
import Events.Destructible
import Events.Events
import Util.WBFiles

main :: IO ()
main =
   do
      parseArguments
      (graph :: Displayable SimpleGraph) <- newEmptyGraph
      graphEditor <- newGraphEditor daVinciSort graph
      putStrLn "Graph displayed"
      let
         graphConnection = shareGraph graph
      (graph2 :: Displayable SimpleGraph) <- newGraph graphConnection
      putStrLn "Graph got"
      graphEditor2 <- newGraphEditor daVinciSort graph2

      sync (destroyed graphEditor)
      sync (destroyed graphEditor2)
      shutdown

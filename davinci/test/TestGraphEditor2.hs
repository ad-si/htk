{- Second test of graph editor -}
module Main(main) where


import GraphEditor
import Graph
import DaVinciGraphDisp
import SimpleGraph
import SIM(destroyed,sync,shutdown)

main =
   do
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
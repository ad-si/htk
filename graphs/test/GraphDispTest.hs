{- GraphDispTest provides a very simple test for a graph implementation,
   basically just to see if all the functions work.  You need to
   call it with the graphs displaySort parameter. -}
module GraphDispTest(
   setUpGraph,
   ) where

setUpGraph :: (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) =>
   (graph,graphParms,node,nodeType,nodeTypeParms,arc,arcType,arcTypeParms) ->
   IO ()
setUpGraph (_:
   (graph,graphParms,node,nodeType,nodeTypeParms,arc,arcType,arcTypeParms)) =
   do
      (graph::graph) <- newGraph [emptyGraphParms :: graphParms]
      

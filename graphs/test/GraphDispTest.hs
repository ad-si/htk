{- GraphDispTest provides a very simple test for a graph implementation,
   basically just to see if all the functions work.  You need to
   call it with the graphs displaySort parameter. -}
module GraphDispTest(
   setUpGraph,
   ) where

import Concurrent

import Debug(debug)

import GraphDisp

setUpGraph :: (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms) 
   => (graph,graphParms,
      node Int,nodeType Int,nodeTypeParms Int,
      arc String Int Int,arcType String,arcTypeParms String) 
   -> IO ()
setUpGraph (_::
   (graph,graphParms,
      node Int,nodeType Int,nodeTypeParms Int,
      arc String Int Int,arcType String,arcTypeParms String) 
      )=
   do
      (graph::graph) <- newGraph (emptyGraphParms :: graphParms)
 
      quitMVar <- newEmptyMVar

      let
         disp s tD = debug (s ++ (show tD))

         (nullNodeParms :: nodeTypeParms Int) = emptyNodeTypeParms
         nodeMenu1 = Button "Type1" (disp "Type1")
         nodeType1Parms = nodeTypeConfig nodeMenu1 nullNodeParms         

         nodeMenu2 = Menu (Just "Type2") [
            Button "Foo" (disp "Type2Foo"),
            Menu Nothing [
               Button "Bah" (disp "Type2Bah"),
               Button "Baz" (disp "Type2Baz"),
               Button "Quit" (putMVar quitMVar)
               ]
            ]
         nodeType2Parms = nodeTypeConfig nodeMenu2 nullNodeParms

      (nodeType1 :: nodeType Int) <- newNodeType graph nodeType1Parms
      (nodeType2 :: nodeType Int) <- newNodeType graph nodeType2Parms

      (nodeA1 :: node Int) <- newNode nodeType1 graph 1
      (nodeB1 :: node Int) <- newNode nodeType1 graph 2
      (nodeC2 :: node Int) <- newNode nodeType2 graph 3

      let
         (nullArcParms :: arcTypeParms String) = emptyArcTypeParms
         arcMenu1 = Button "ArcType1" (disp "ArcType1")
         arcType1Parms = arcTypeConfig arcMenu1 nullArcParms         
      (arcType1 :: arcType String) <- newArcType graph arcType1Parms

      (arcA :: arc String Int Int) <- 
         newArc arcType1 graph "Arc A" nodeC2 nodeA1
      (arcB :: arc String Int Int) <-
         newArc arcType1 graph "Arc B" nodeC2 nodeB1
   
      redraw graph

      quitNo <- takeMVar quitMVar

      debug quitNo



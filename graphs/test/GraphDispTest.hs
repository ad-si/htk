{- GraphDispTest provides a very simple test for a graph implementation,
   basically just to see if all the functions work.  You need to
   call it with the graphs displaySort parameter. -}
module GraphDispTest(
   setUpGraph,
   ) where

import Concurrent

import Debug(debug)

import Selective
import SIM(Destructible(..),lift)

import GraphDisp
import GraphConfigure

setUpGraph :: 
   (GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,
    HasConfig GraphTitle graphParms,
    HasConfig OptimiseLayout graphParms,
    HasConfigValue LocalMenu nodeTypeParms,
    HasConfigValue Shape nodeTypeParms,
    HasConfigValue ValueTitle nodeTypeParms,
    HasConfigValue LocalMenu arcTypeParms
    ) 
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
      let
         nullGraphParms = emptyGraphParms :: graphParms
      if (configUsed (GraphTitle "") nullGraphParms)
         then
            return ()
         else
            error "Graph Title config is ignored!"
      let
         (graphParms :: graphParms) = 
            GraphTitle "Test Graph Display" $$
            OptimiseLayout True $$ 
            nullGraphParms 
      (graph::graph) <- newGraph graphParms

      (killChannel :: Channel ()) <- newChannel

      stringMVar <- newMVar ""

      let
         disp s tD = debug (s ++ (show tD))
         (nullNodeParms :: nodeTypeParms Int) = emptyNodeTypeParms

         nodeMenu1 = LocalMenu(Button "Type1" (disp "Type1"))   
         nodeType1Parms = 
            nodeMenu1 $$$ 
            Rhombus $$$
            ValueTitle (\ value -> return ("Type 1"++show value)) $$$ 
            nullNodeParms

         nodeMenu2 = LocalMenu(Menu(Just "Type2") [
            Button "Foo" (disp "Type2Foo"),
            Menu Nothing [
               Button "Bah" (disp "Type2Bah"),
               Button "Baz" (disp "Type2Baz"),
               Button "Quit" (\ _ -> sendIO killChannel ())
               ]
            ])

         nodeType2Parms =
            nodeMenu2 $$$
            Icon "mawe.xbm" $$$
            ValueTitle (\ _ -> return "Type 2") $$$ 
            nullNodeParms

         buttonChar =
            LocalMenu(Button "In" 
               (\ char -> do
                  str <- takeMVar stringMVar
                  putMVar stringMVar (str++[char])
                  ))

         (nodeTypeCharParms :: nodeTypeParms Char) =
            buttonChar $$$
            Triangle $$$
            ValueTitle (
               \ char -> return [char]
               ) $$$ emptyNodeTypeParms

         buttonWrite =
            LocalMenu(Button "Write"
               (\ () -> do
                  str <- takeMVar stringMVar
                  putStrLn str
                  putMVar stringMVar ""
                  ))

         (nodeTypeWriteParms :: nodeTypeParms ()) =
            buttonWrite $$$
            Ellipse $$$
            ValueTitle (\ _ -> return "Write") $$$ emptyNodeTypeParms

         (nodeTypeSmallParms :: nodeTypeParms ()) =
            LocalMenu (Menu Nothing []) $$$ emptyNodeTypeParms

      (nodeType1 :: nodeType Int) <- newNodeType graph nodeType1Parms
      (nodeType2 :: nodeType Int) <- newNodeType graph nodeType2Parms
      (nodeTypeChar :: nodeType Char) <- newNodeType graph nodeTypeCharParms
      (nodeTypeWrite :: nodeType ()) <- newNodeType graph nodeTypeWriteParms
      (nodeTypeSmall :: nodeType ()) <- newNodeType graph nodeTypeSmallParms

      (nodeA1 :: node Int) <- newNode nodeType1 graph 1
      (nodeB1 :: node Int) <- newNode nodeType1 graph 2
      (nodeC2 :: node Int) <- newNode nodeType2 graph 3

      (nodeChars :: [node Char]) <- mapM (newNode nodeTypeChar graph) 
         "0123456789"
      (nodeWrite :: node ()) <- newNode nodeTypeWrite graph ()

      let
         (nullArcParms :: arcTypeParms String) = emptyArcTypeParms
         arcMenu1 = LocalMenu(Button "ArcType1" (disp "ArcType1"))

         arcType1Parms = arcMenu1 $$$ nullArcParms

      (arcType1 :: arcType String) <- newArcType graph arcType1Parms

      (arcA :: arc String Int Int) <- 
         newArc arcType1 graph "Arc A" nodeC2 nodeA1
      (arcB :: arc String Int Int) <-
         newArc arcType1 graph "Arc B" nodeC2 nodeB1

      (arcWrite :: arc String Int ()) <-
          newArc arcType1 graph "" nodeA1 nodeWrite

      (arcChars :: [arc String Int Char]) <-
         mapM (newArc arcType1 graph "" nodeA1) nodeChars

      redraw graph

      sync(
            (lift(receive killChannel) >>> 
               do
                  putStrLn "Destroy graph"
                  destroy graph
               )
         +> (destroyed graph)
         )

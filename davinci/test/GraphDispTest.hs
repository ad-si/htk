-- | GraphDispTest provides a very simple test for a graph implementation,
-- basically just to see if all the functions work.  You need to
-- call it with the graphs displaySort parameter.
module GraphDispTest(
   setUpGraph,
   ) where

import Control.Concurrent

import Util.Debug(debug)

import Events.Events
import Events.Destructible
import Events.Channels

import Graphs.GraphDisp as GraphDisp
import Graphs.GraphConfigure

setUpGraph ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> IO ()
setUpGraph
   (displaySort ::
       GraphDisp.Graph graph graphParms node nodeType nodeTypeParms arc
          arcType arcTypeParms) =
   do
      let
         graphParms  =
            GraphTitle "Test Graph Display" $$
            OptimiseLayout True $$
            emptyGraphParms
      graph <- newGraph displaySort graphParms

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
            Color "light steel blue" $$$
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
            Color "purple" $$$
            nullNodeParms

         buttonChar =
            LocalMenu(Button "In"
               (\ char -> do
                  str <- takeMVar stringMVar
                  putMVar stringMVar (str++[char])
                  ))

         (nodeTypeCharParms :: nodeTypeParms Char) =
            buttonChar $$$
            DoubleClickAction (\ char -> putStrLn ("clicked "++[char])) $$$
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

      nodeType1  <- newNodeType graph nodeType1Parms
      nodeType2 <- newNodeType graph nodeType2Parms
      nodeTypeChar <- newNodeType graph nodeTypeCharParms
      nodeTypeWrite <- newNodeType graph nodeTypeWriteParms
      nodeTypeSmall <- newNodeType graph nodeTypeSmallParms

      nodeA1 <- newNode graph nodeType1 1
      nodeB1 <- newNode graph nodeType1 2
      nodeC2 <- newNode graph nodeType2 3

      (nodeChars :: [node Char]) <- mapM (newNode graph nodeTypeChar)
         "0123456789"
      nodeWrite  <- newNode graph nodeTypeWrite ()

      let
         arcMenu1 = LocalMenu(Button "ArcType1" (disp "ArcType1"))

         arcType1Parms =
            arcMenu1 $$$
            Color "red" $$$
            emptyArcTypeParms

         arcType2Parms =
            arcMenu1 $$$
            Dotted $$$
            emptyArcTypeParms

      arcType1 <- newArcType graph arcType1Parms
      arcType2 <- newArcType graph arcType2Parms

      arcA <- newArc graph arcType1 "Arc A" nodeC2 nodeA1
      arcB <- newArc graph arcType1 "Arc B" nodeC2 nodeB1

      arcWrite <- newArc graph arcType1 "" nodeA1 nodeWrite

      (arcChars :: [arc String]) <-
         mapM (newArc graph arcType2 "" nodeA1) nodeChars

      redraw graph

      sync(
            (receive killChannel) >>>
               do
                  putStrLn "Destroy graph"
                  destroy graph

         +> (destroyed graph)
         )

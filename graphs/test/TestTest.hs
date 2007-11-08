
module Main(main) where


{- Test file for graph package. -}

import Concurrent

import Debug(debug)

import Selective
import SIM(Destructible(..),lift)

import GraphDisp
import GraphConfigure

import DaVinciGraphDisp
import SIM(shutdown)
import WBFiles(parseArguments)


setUpGraph (displaySort) =
   do

      -- Create new graph

      let
         graphParms  =
            GraphTitle "Test Graph Display" $$
            OptimiseLayout True $$
            emptyGraphParms
      graph <- newGraph displaySort graphParms

      (killChannel :: Channel ()) <- newChannel


      -- Setup node and egde types

      let
         nodeType1Parms =
            LocalMenu (Menu Nothing []) $$$ emptyNodeTypeParms

         arcType1Parms =
            LocalMenu (Menu Nothing []) $$$ emptyArcTypeParms

      nodeType1 <- newNodeType graph nodeType1Parms

      arcType1 <- newArcType graph arcType1Parms


      -- Create nodes and edges

      nodeA <- newNode graph nodeType1 ()
      nodeB <- newNode graph nodeType1 ()
      nodeC <- newNode graph nodeType1 ()

      arcA <- newArc graph arcType1 "Arc A" nodeA nodeB
      arcB <- newArc graph arcType1 "Arc B" nodeB nodeC
      arcC <- newArc graph arcType1 "" nodeC nodeA


      -- Draw graph

      redraw graph


      -- Exit

      sync(
            (lift(receive killChannel) >>>
               do
                  putStrLn "Destroy graph"
                  destroy graph
               )
         +> (destroyed graph)
         )

main =
   do
      parseArguments
      setUpGraph daVinciSort
      shutdown

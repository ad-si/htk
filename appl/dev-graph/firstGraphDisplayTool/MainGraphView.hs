module Main(main) where

import Concurrent

import Debug(debug)

import Events
import Destructible
import Channels

import DaVinciGraph

import WBFiles(parseArguments)
import InfoBus(shutdown)
import qualified IOExts(unsafePerformIO)

import GraphDisp
import GraphConfigure

import Graph
import Parser
import Paths

import System

instance Eq (DaVinciNode a) 
  where x == y = False

   
setUpGraph (displaySort:: GraphDisp.Graph graph graphParms node nodeType nodeTypeParms arc 
          arcType arcTypeParms, readGraph :: Graph.Graph, actGraph :: Graph.Graph) =
 do
 
      -- Create new graph
  
      let
         graphParms  = 
            GraphTitle "Test Graph Display" $$
            GlobalMenu (
               Menu (Just "View") [
                  Button "Hide internal nodes" (
                     do
			setUpGraph(displaySort, readGraph, (hideInternalNodes actGraph))),
                  Button "Add all paths" (
		     do
		        setUpGraph(displaySort, readGraph, (allPaths actGraph)))
                  ]
	       )  $$
	       
            OptimiseLayout True $$ 
	    emptyGraphParms 
      graph <- GraphDisp.newGraph displaySort graphParms
   
      (killChannel :: Channel ()) <- newChannel


      -- Setup node and egde types

      let
	 -- Internal node
         nodeType1Parms =
	    LocalMenu (Menu Nothing []) $$$            
	    Rhombus  $$$
            ValueTitle ( \ s -> return s) $$$
            emptyNodeTypeParms
	    
	-- Defined node
	 nodeType2Parms =
	    LocalMenu (Menu Nothing []) $$$
	    Ellipse $$$
            ValueTitle ( \ s -> return s) $$$
            emptyNodeTypeParms

         arcMenu = LocalMenu(
	              Button "Restore edges" 
	                 (\ edge -> do
	                               setUpGraph(displaySort, readGraph, (restoreEdges readGraph actGraph edge))))

	 -- axiom, global
         (arcType1Parms :: arcTypeParms (NodeType, (EdgeType, NodeType))) = 
	    arcMenu $$$
	    Color "black" $$$
	    emptyArcTypeParms

	 -- axiom, local
	 (arcType2Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
            -- LocalMenu (Menu Nothing []) $$$
	    Color "black" $$$
	    Dotted $$$
	    emptyArcTypeParms

	 -- axiom, mixedLocType
	 (arcType3Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "black" $$$
	    Dashed $$$
	    emptyArcTypeParms

	 -- theorem, global | mixedThmType, global
	 (arcType4Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "green" $$$
	    emptyArcTypeParms

	 -- theorem, local | mixedThmType, local
	 (arcType5Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "green" $$$
	    Dotted $$$
	    emptyArcTypeParms

	 -- theorem, mixedLocType | mixedThmType, mixedLocType
	 (arcType6Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "green" $$$
	    Dashed $$$
	    emptyArcTypeParms

         -- proofObligation, global | mixedProofObl, global
	 (arcType7Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "red" $$$
	    emptyArcTypeParms
	    
         -- proofObligation, local | mixedProofObl, local
	 (arcType8Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "red" $$$
	    Dotted $$$
	    emptyArcTypeParms

         -- proofObligation, mixedLocType | mixedProofObl, mixedLocType
	 (arcType9Parms :: arcTypeParms (NodeType, (EdgeType, NodeType)))  =
	    arcMenu $$$
	    Color "red" $$$
	    Dashed $$$
	    emptyArcTypeParms


      nodeType1 <- newNodeType graph nodeType1Parms

      nodeType2 <- newNodeType graph nodeType2Parms

      arcType1 <- newArcType graph arcType1Parms

      arcType2 <- newArcType graph arcType2Parms

      arcType3 <- newArcType graph arcType3Parms

      arcType4 <- newArcType graph arcType4Parms
     
      arcType5 <- newArcType graph arcType5Parms
     
      arcType6 <- newArcType graph arcType6Parms
     
      arcType7 <- newArcType graph arcType7Parms

      arcType8 <- newArcType graph arcType8Parms

      arcType9 <- newArcType graph arcType9Parms



      -- Create nodes and edges

      let

          -- determines by the nodetype in the Graph.Graph
          -- the corresponding nodetype in the new graph
          getTypeOfNode nd = case (getNodeType nd) of
	     "Internal" -> nodeType1
	     "Defined"  -> nodeType2

          -- determines by the edgetype in the Graph.Graph
          -- the corresponding edgetype in the new graph
          getTypeOfArc arc = case (getEdgeType arc) of
	     "(Axiom,GlobalLink)"		 -> arcType1
	     "(Axiom,LocalLink)"		 -> arcType2
	     "(Axiom,MixedLocType)"	         -> arcType3
	     "(Theorem,GlobalLink)"              -> arcType4
	     "(MixedThmType,GlobalLink)"         -> arcType4
	     "(Theorem,LocalLink)"               -> arcType5
	     "(MixedThmType,LocalLink)"          -> arcType5
	     "(Theorem,MixedLocType)"            -> arcType6
	     "(MixedThmType,MixedLocType)"       -> arcType6
	     "(ProofObligation,GlobalLink)"	 -> arcType7
	     "(MixedProofObl,GlobalLink)"	 -> arcType7
	     "(ProofObligation,LocalLink)"	 -> arcType8
	     "(MixedProofObl,LocalLink)"	 -> arcType8
	     "(ProofObligation,MixedLocType)"    -> arcType9
	     "(MixedProofObl,MixedLocType)"      -> arcType9

          -- looks up the partnernode of a node of the Graph.Graph,
          -- i.e. the corresponding node in the new graph
          getPartnerNode node list = if (list /= []) then
	                                  if ((snd (head list)) == node) then (fst (head list))
		 		          else getPartnerNode node (tail list)
			             else error "getPartnerNode: no such node"
          mk_node node = 
             do n<-newNode graph (getTypeOfNode node) (read (getNodeName node))
                return (n,node)

      -- creates nodes, while building a list of pairs of Graph.Graph nodes
      -- and their partnernodes
      nodeList <- sequence [mk_node node| (node,_) <- (fromGraph actGraph)]

      let
          -- adds all edges from the Graph.Graph
	  edges remGraph gr =  do if (remGraph /= []) then
			             do edge (head remGraph) gr
		                        edges (tail remGraph) gr
		                   else return ()

          -- adds all edges beginning at the same node
          edge eds@(node,list) gr = do if (list /= []) then
	                                  do newArc gr (getTypeOfArc(fst(head list)))
					            (node,(fst(head list),snd(head list)))
					            (getPartnerNode node nodeList) 
                                                    (getPartnerNode (snd(head list)) nodeList)
			                     edge (node,(tail list)) gr
			                else return ()
				       
      -- Create edges

      edges (fromGraph actGraph) graph

      -- Draw graph

      redraw graph

      -- Exit

      sync(
            (receive killChannel) >>> 
               do
                  putStrLn "Destroy graph"
                  destroy graph
               
         +> (destroyed graph)
         )


      
main = 
   do
      parseArguments      
      graphFile <- getArgs
      readGraph <- readStructure (head graphFile)
      setUpGraph (daVinciSort,readGraph,readGraph)
      shutdown












{- DisplayGraph.displayGraph displays something implementing the
   Graph interface with something implementing with GraphDisp interface.
   -}
module DisplayGraph(
   displayGraph
   ) where

import Concurrent(forkIO)
import Dynamics
import Registry
import Computation (done)

import Selective

import SIM(destroyed,lift)

import GraphDisp
import Graph


displayGraph ::
      (GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,
       Typeable nodeLabel,Typeable nodeTypeLabel,Typeable arcLabel,
       Typeable arcTypeLabel)
   => (graph,graphParms,
      node (Graph.Node,nodeLabel),
         nodeType (Graph.Node,nodeLabel),nodeTypeParms (Graph.Node,nodeLabel),
      arc (Graph.Arc,arcLabel) (Graph.Node,nodeLabel) (Graph.Node,nodeLabel),
         arcType (Graph.Arc,arcLabel),arcTypeParms (Graph.Arc,arcLabel)
         )
   -> (GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> graphParms -- these are the parameters to use setting up the graph
   -> ((Graph.NodeType,nodeTypeLabel) 
          -> IO (nodeTypeParms (Graph.Node,nodeLabel)))
                 -- this gets parameters for setting up a node type.
                 -- NB - we don't (and can't) recompute the parameters
                 -- if we get a SetNodeTypeLabel or SetArcTypeLabel update.
   -> ((Graph.ArcType,arcTypeLabel) 
         -> IO (arcTypeParms (Graph.Arc,arcLabel)))
   -> IO ()
displayGraph (_::
   (graph,graphParms,
      node (Graph.Node,nodeLabel),
         nodeType (Graph.Node,nodeLabel),nodeTypeParms (Graph.Node,nodeLabel),
      arc (Graph.Arc,arcLabel) (Graph.Node,nodeLabel) (Graph.Node,nodeLabel),
         arcType (Graph.Arc,arcLabel),arcTypeParms (Graph.Arc,arcLabel)
         ))
   (graphConnection :: 
      GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   graphParms 
   (getNodeParms :: (Graph.NodeType,nodeTypeLabel) 
      -> IO (nodeTypeParms (Graph.Node,nodeLabel)))
   (getArcParms :: (Graph.ArcType,arcTypeLabel) 
      -> IO (arcTypeParms (Graph.Arc,arcLabel))) =
   do
-- Since Haskell doesn't allow type definitions except at top
-- level, we are forced to use C macros . . .
#define DispNodeType (nodeType (Graph.Node,nodeLabel))
#define DispNode (node (Graph.Node,nodeLabel))
#define DispArcType (arcType (Graph.Arc,arcLabel))
#define DispArc (arc (Graph.Arc,arcLabel) (Graph.Node,nodeLabel) \
    (Graph.Node,nodeLabel))

      msgQueue <- newMsgQueue
      GraphConnectionData {
         graphState = CannedGraph { updates = updates },
         deRegister = deRegister
         } <- graphConnection (sendIO msgQueue)
 
      (nodeRegister :: Registry Graph.Node DispNode) <- newRegistry
      (nodeTypeRegister :: Registry Graph.NodeType DispNodeType)
         <- newRegistry
      (arcRegister :: Registry Graph.Arc DispArc) <- newRegistry
      (arcTypeRegister :: Registry Graph.ArcType DispArcType)
         <- newRegistry

      (graph :: graph) <- GraphDisp.newGraph graphParms

      let
         handleUpdate :: Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
           -> IO ()
         handleUpdate (NewNodeType nodeType nodeTypeLabel) =
            do
               nodeTypeParms <- getNodeParms (nodeType,nodeTypeLabel)
               (dispNodeType :: DispNodeType) 
                  <- GraphDisp.newNodeType graph nodeTypeParms
               setValue nodeTypeRegister nodeType dispNodeType
         handleUpdate (SetNodeTypeLabel _ _ ) = done
         handleUpdate (NewNode node nodeType nodeLabel) =
            do
               (dispNodeType :: DispNodeType) 
                  <- getValue nodeTypeRegister nodeType
               (dispNode :: DispNode) 
                  <- GraphDisp.newNode dispNodeType graph (node,nodeLabel)
               setValue nodeRegister node dispNode
         handleUpdate (DeleteNode node) =
            do
               (dispNode :: DispNode) <- getValue nodeRegister node
               deleteNode graph dispNode
               deleteFromRegistry nodeRegister node
         handleUpdate (SetNodeLabel node nodeLabel) =
            do
               (dispNode :: DispNode) <- getValue nodeRegister node
               setNodeValue graph dispNode (node,nodeLabel)
         handleUpdate (NewArcType arcType arcTypeLabel) =
            do
               arcTypeParms <- getArcParms (arcType,arcTypeLabel)
               (dispArcType :: DispArcType)
                  <- GraphDisp.newArcType graph arcTypeParms
               setValue arcTypeRegister arcType dispArcType
         handleUpdate (SetArcTypeLabel _ _) = done
         handleUpdate (NewArc arc arcType arcLabel source target) =
            do
               (dispSource :: DispNode) <- getValue nodeRegister source
               (dispTarget :: DispNode) <- getValue nodeRegister target
               (dispArcType :: DispArcType) 
                  <- getValue arcTypeRegister arcType
               (dispArc :: DispArc) <- GraphDisp.newArc dispArcType graph 
                  (arc,arcLabel) dispSource dispTarget
               setValue arcRegister arc dispArc
         handleUpdate (DeleteArc arc) =
            do
               (dispArc :: DispArc) <- getValue arcRegister arc
               deleteArc graph dispArc
               deleteFromRegistry arcRegister arc
         handleUpdate (SetArcLabel arc arcLabel) =
            do
               (dispArc :: DispArc) <- getValue arcRegister arc
               setArcValue graph dispArc (arc,arcLabel)
      sequence_ (map handleUpdate updates)

      redraw graph

      let
         getAllQueued =
            do
               updateOpt <- poll (receive msgQueue)
               case updateOpt of
                  Nothing -> done
                  Just update ->
                     do
                        handleUpdate update
                        getAllQueued
         monitorThread =
            sync(
                  (lift (receive msgQueue)) >>>=
                     (\ update ->
                        do
                           handleUpdate update
                           getAllQueued
                           redraw graph
                           monitorThread
                         )
               +> (destroyed graph) >>> (
                     do
                        deRegister
                        done
                     )
               )

      forkIO monitorThread    
      done              





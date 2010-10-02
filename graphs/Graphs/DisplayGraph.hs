{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'displayGraph' displays something implementing the
-- "Graph" interface with something implementing with "GraphDisp" interface.
-- 'displayGraph0' is a slightly more general version that also returns the
-- actual graph.
module Graphs.DisplayGraph(
   displayGraph,
   displayGraph0,
   displayGraph1,

   DisplayGraph
   ) where

import Control.Concurrent(forkIO)

import Util.Dynamics
import Util.Registry
import Util.Computation (done)
import Util.Object

import Reactor.InfoBus

import Events.Events
import Events.Channels
import Events.Destructible

import qualified Graphs.GraphDisp as GraphDisp
    (Graph, newGraph, newNode, newNodeType, newArc, newArcType)
import Graphs.GraphDisp hiding
    (Graph, newGraph, newNode, newNodeType, newArc, newArcType)
import qualified Graphs.Graph as Graph (Graph)
import Graphs.Graph hiding (Graph)

#ifdef DEBUG
#define getRegistryValue (getRegistryValueSafe (__FILE__ ++ show (__LINE__)))
#endif

displayGraph ::
      (GraphAll dispGraph graphParms node nodeType nodeTypeParms
            arc arcType arcTypeParms,
         Typeable nodeLabel,Typeable nodeTypeLabel,Typeable arcLabel,
         Typeable arcTypeLabel,
         Graph.Graph graph)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
          arc arcType arcTypeParms)
   -> (graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> graphParms -- these are the parameters to use setting up the graph
   -> (DisplayGraph -> NodeType -> nodeTypeLabel
          -> IO (nodeTypeParms Node))
                 -- this gets parameters for setting up a node type.
                 -- NB - we don't (and can't) recompute the parameters
                 -- if we get a SetNodeTypeLabel or SetArcTypeLabel update.
                 -- We provide the function with the DisplayGraph
                 -- this function will return, to make tying the knot easier
                 -- in versions/VersionGraph.hs
   -> (DisplayGraph -> ArcType -> arcTypeLabel
         -> IO (arcTypeParms Arc))
                 -- see previous argument.
   -> IO DisplayGraph
displayGraph displaySort graph graphParms getNodeParms getArcParms =
   do
      (displayedGraph,_) <- displayGraph0 displaySort graph graphParms
         getNodeParms getArcParms
      return displayedGraph


displayGraph0 ::
      (GraphAll dispGraph graphParms node nodeType nodeTypeParms
            arc arcType arcTypeParms,
         Typeable nodeLabel,Typeable nodeTypeLabel,Typeable arcLabel,
         Typeable arcTypeLabel,
         Graph.Graph graph)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
          arc arcType arcTypeParms)
   -> (graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> graphParms -- these are the parameters to use setting up the graph
   -> (DisplayGraph -> NodeType -> nodeTypeLabel
          -> IO (nodeTypeParms Node))
                 -- this gets parameters for setting up a node type.
                 -- NB - we don't (and can't) recompute the parameters
                 -- if we get a SetNodeTypeLabel or SetArcTypeLabel update.
                 -- We provide the function with the DisplayGraph
                 -- this function will return, to make tying the knot easier
                 -- in versions/VersionGraph.hs
   -> (DisplayGraph -> ArcType -> arcTypeLabel
         -> IO (arcTypeParms Arc))
                 -- see previous argument.
   -> IO (DisplayGraph,GraphDisp.Graph dispGraph graphParms
      node nodeType nodeTypeParms arc arcType arcTypeParms)
displayGraph0 displaySort
   (graph :: graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   graphParms
   (getNodeParms0 :: DisplayGraph -> NodeType -> nodeTypeLabel
      -> IO (nodeTypeParms Node))
   (getArcParms0 :: DisplayGraph -> ArcType -> arcTypeLabel
      -> IO (arcTypeParms Arc)) =
   let
      getNodeParms1 :: DisplayGraph -> NodeType -> nodeTypeLabel
         -> IO (nodeTypeParms (Node,nodeLabel))
      getNodeParms1 graph nodeType nodeTypeLabel =
         do
            nodeParms0 <- getNodeParms0 graph nodeType nodeTypeLabel
            return (coMapNodeTypeParms fst nodeParms0)

      getArcParms1 :: DisplayGraph -> ArcType -> arcTypeLabel
         -> IO (arcTypeParms (Arc,arcLabel))
      getArcParms1 graph arcType arcTypeLabel =
         do
            arcParms0 <- getArcParms0 graph arcType arcTypeLabel
            return (coMapArcTypeParms fst arcParms0)
   in
      displayGraph1 displaySort (shareGraph graph) graphParms getNodeParms1
         getArcParms1

displayGraph1 ::
      (GraphAll dispGraph graphParms node nodeType nodeTypeParms
            arc arcType arcTypeParms,
         Typeable nodeLabel,Typeable nodeTypeLabel,Typeable arcLabel,
         Typeable arcTypeLabel)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
          arc arcType arcTypeParms)
   -> (GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> graphParms -- these are the parameters to use setting up the graph
   -> (DisplayGraph -> NodeType -> nodeTypeLabel
          -> IO (nodeTypeParms (Node,nodeLabel)))
                 -- this gets parameters for setting up a node type.
                 -- NB - we don't (and can't) recompute the parameters
                 -- if we get a SetNodeTypeLabel or SetArcTypeLabel update.
                 -- We provide the function with the DisplayGraph
                 -- this function will return, to make tying the knot easier
                 -- in versions/VersionGraph.hs
   -> (DisplayGraph -> ArcType -> arcTypeLabel
         -> IO (arcTypeParms (Arc,arcLabel)))
                 -- see previous argument.
   -> IO (DisplayGraph,GraphDisp.Graph dispGraph graphParms
      node nodeType nodeTypeParms arc arcType arcTypeParms)
displayGraph1
   (displaySort ::
       GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms arc
          arcType arcTypeParms)
   (graphConnection
      :: GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   graphParms
   (getNodeParms :: DisplayGraph -> NodeType -> nodeTypeLabel
      -> IO (nodeTypeParms (Node,nodeLabel)))
   (getArcParms :: DisplayGraph -> ArcType -> arcTypeLabel
      -> IO (arcTypeParms (Arc,arcLabel))) =
   do
      msgQueue <- newChannel

      GraphConnectionData {
         graphState = CannedGraph { updates = updates },
         deRegister = deRegister
         } <- graphConnection (sync. noWait . (send msgQueue))

-- The nodes of the graph display will have the following types:
#define DispNodeType (nodeType (Node,nodeLabel))
#define DispNode (node (Node,nodeLabel))
#define DispArcType (arcType (Arc,arcLabel))
#define DispArc (arc (Arc,arcLabel))

      (nodeRegister :: Registry Node DispNode) <- newRegistry
      (nodeTypeRegister :: Registry NodeType DispNodeType)
         <- newRegistry
      (arcRegister :: Registry Arc DispArc) <- newRegistry
      (arcTypeRegister :: Registry ArcType DispArcType)
         <- newRegistry

      dispGraph <- GraphDisp.newGraph displaySort graphParms

      (destructionChannel :: Channel ()) <- newChannel

      oID <- newObject

      let
         displayGraph = DisplayGraph {
            oID = oID,
            destroyAction = destroy dispGraph,
            destroyedEvent = receive destructionChannel
            }

         handleUpdate :: Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
           -> IO ()
         handleUpdate (NewNodeType nodeType nodeTypeLabel) =
            do
               nodeTypeParms <-
                  getNodeParms displayGraph nodeType nodeTypeLabel
               dispNodeType <- GraphDisp.newNodeType dispGraph nodeTypeParms
               setValue nodeTypeRegister nodeType dispNodeType
         handleUpdate (SetNodeTypeLabel _ _ ) = done
         handleUpdate (NewNode node nodeType nodeLabel) =
            do
               dispNodeType <- getRegistryValue nodeTypeRegister nodeType
               dispNode <-
                  GraphDisp.newNode dispGraph dispNodeType (node,nodeLabel)
               setValue nodeRegister node dispNode
         handleUpdate (DeleteNode node) =
            do
               dispNode <- getRegistryValue nodeRegister node
               deleteNode dispGraph dispNode
               deleteFromRegistry nodeRegister node
         handleUpdate (SetNodeLabel node nodeLabel) =
            do
               dispNode <- getRegistryValue nodeRegister node
               setNodeValue dispGraph dispNode (node,nodeLabel)
         handleUpdate (SetNodeType node nodeType) =
            do
               dispNode <- getRegistryValue nodeRegister node
               dispNodeType <- getRegistryValue nodeTypeRegister nodeType
               setNodeType dispGraph dispNode dispNodeType
         handleUpdate (NewArcType arcType arcTypeLabel) =
            do
               arcTypeParms <-
                  getArcParms displayGraph arcType arcTypeLabel
               dispArcType <- GraphDisp.newArcType dispGraph arcTypeParms
               setValue arcTypeRegister arcType dispArcType
         handleUpdate (SetArcTypeLabel _ _) = done
         handleUpdate (NewArc arc arcType arcLabel source target) =
            do
               dispSource <- getRegistryValue nodeRegister source
               dispTarget <- getRegistryValue nodeRegister target
               dispArcType <- getRegistryValue arcTypeRegister arcType
               dispArc <- GraphDisp.newArc dispGraph dispArcType
                  (arc,arcLabel) dispSource dispTarget
               setValue arcRegister arc dispArc
         handleUpdate (DeleteArc arc) =
            do
               dispArc <- getRegistryValue arcRegister arc
               deleteArc dispGraph dispArc
               deleteFromRegistry arcRegister arc
         handleUpdate (SetArcLabel arc arcLabel) =
            do
               dispArc <- getRegistryValue arcRegister arc
               setArcValue dispGraph dispArc (arc,arcLabel)
         handleUpdate (MultiUpdate updates) = mapM_ handleUpdate updates

      sequence_ (map handleUpdate updates)

      redraw dispGraph

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

      let
         monitorThread =
            sync(
                  (receive msgQueue) >>>=
                     (\ update ->
                        do
                           handleUpdate update
                           getAllQueued
                           redraw dispGraph
                           monitorThread
                         )
               +> (destroyed dispGraph) >>> (
                     do
                        deregisterTool displayGraph
                        deRegister
                        sendIO destructionChannel ()
                     )
               )

      forkIO monitorThread

      registerToolDebug "DisplayGraph" displayGraph

      return (displayGraph,dispGraph)

--------------------------------------------------------------------
-- The DisplayGraph type.  (We create this so that we can end
-- the display tidily.)
--------------------------------------------------------------------

data DisplayGraph = DisplayGraph {
   oID :: ObjectID,
   destroyAction :: IO (), -- run this to end everything
   destroyedEvent :: Event ()
   }

instance Object DisplayGraph where
   objectID displayGraph = oID displayGraph


instance Destroyable DisplayGraph where
   destroy displayGraph = destroyAction displayGraph

instance Destructible DisplayGraph where
   destroyed displayGraph = destroyedEvent displayGraph





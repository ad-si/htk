{- Here we fit DaVinci into the GraphDisp framework. -}
module DaVinciGraphDisp(
   DaVinciGraph,
   DaVinciGraphParms,
   DaVinciNode,
   DaVinciNodeType,
   DaVinciNodeTypeParms,
   DaVinciArc,
   DaVinciArcType,
   DaVinciArcTypeParms
   ) where

import FiniteMap

import Computation
import Concurrent
import Dynamics

import SIM(destroy)

import qualified DaVinci

import GraphDisp


-- We follow the order of the GraphDisp file.

------------------------------------------------------------------------
-- Graphs
------------------------------------------------------------------------

data DaVinciGraph = 
   DaVinciGraph DaVinci.Graph 
      (MVar(FiniteMap DaVinci.Node Dyn)) (MVar(FiniteMap DaVinci.Edge Dyn))

newtype DaVinciGraphParms = DaVinciGraphParms [Config DaVinci.Graph]

instance Graph DaVinciGraph where
   redraw (DaVinciGraph graph _ _) = DaVinci.redrawGraph graph

instance NewGraph DaVinciGraph DaVinciGraphParms where
   newGraph (DaVinciGraphParms graphParms) =
      do
         graph <- DaVinci.newGraph graphParms
         nodeLabelMVar <- newMVar emptyFM
         arcLabelMVar <- newMVar emptyFM
         return (DaVinciGraph graph nodeLabelMVar arcLabelMVar)

instance GraphParms DaVinciGraphParms where
   emptyGraphParms = DaVinciGraphParms []

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

newtype DaVinciNode = DaVinciNode DaVinci.Node

newtype DaVinciNodeType = DaVinciNodeType DaVinci.NodeType

data DaVinciNodeTypeParms = 
   DaVinciNodeTypeParms (Maybe String) [Config DaVinci.NodeType]

instance NewNode DaVinciGraph DaVinciNode DaVinciNodeType where
   newNodePrim (DaVinciNodeType nodeType) (DaVinciGraph graph nodeLabelMVar _)
         dyn =
      do
         nodeLabelMap <- takeMVar nodeLabelMVar
         node <- DaVinci.newNode graph Nothing [DaVinci.nodetype nodeType] 
         putMVar nodeLabelMVar (addToFM nodeLabelMap node dyn)
         return (DaVinciNode node)
   readNodePrim (DaVinciGraph graph nodeLabelMVar _) (DaVinciNode node) =
      do
         nodeLabelMap <- takeMVar nodeLabelMVar
         let
            Just dyn = lookupFM nodeLabelMap node
            -- fail error here probably means node isn't actually in graph.
         putMVar nodeLabelMVar nodeLabelMap
         nodeType <- DaVinci.getNodeType node
         return (DaVinciNodeType nodeType,dyn)

instance DeleteNode DaVinciGraph DaVinciNode where
   deleteNode (DaVinciGraph _ nodeLabelMVar _) (DaVinciNode node) = 
      do
         nodeLabelMap <- takeMVar nodeLabelMVar 
         destroy node
         putMVar nodeLabelMVar (delFromFM nodeLabelMap node)

instance Node DaVinciNode where

instance NodeType DaVinciNodeType where

instance NewNodeType DaVinciGraph DaVinciNodeType DaVinciNodeTypeParms where
   newNodeType (DaVinciGraph graph _ _) 
         (DaVinciNodeTypeParms titleOpt configList) =
      do
         nodeType <- DaVinci.newNodeType graph titleOpt configList
         return (DaVinciNodeType nodeType)

instance NodeTypeParms DaVinciNodeTypeParms where
   emptyNodeTypeParms = DaVinciNodeTypeParms Nothing []

------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

newtype DaVinciArc = DaVinciArc DaVinci.Edge

newtype DaVinciArcType = DaVinciArcType DaVinci.EdgeType

data DaVinciArcTypeParms = 
   DaVinciArcTypeParms (Maybe String) [Config DaVinci.EdgeType]

instance NewArc DaVinciGraph DaVinciNode DaVinciNode DaVinciArc DaVinciArcType
      where
   newArcPrim (DaVinciArcType edgeType) (DaVinciGraph graph _ arcLabelMVar)
         (DaVinciNode nodeFrom) (DaVinciNode nodeTo) dyn =
      do
         arcLabelMap <- takeMVar arcLabelMVar         
         edge <- DaVinci.newEdge Nothing nodeFrom nodeTo 
            [DaVinci.edgetype edgeType]
         putMVar arcLabelMVar (addToFM arcLabelMap edge dyn)
         return (DaVinciArc edge)
   readArcPrim (DaVinciGraph graph _ arcLabelMVar) (DaVinciArc edge) =
      do
         arcLabelMap <- takeMVar arcLabelMVar
         let
            Just dyn = lookupFM arcLabelMap edge
            -- fail error here probably means edge isn't actually in graph.
         putMVar arcLabelMVar arcLabelMap
         edgeType <- DaVinci.getEdgeType edge
         nodeFrom <- DaVinci.getSource edge
         nodeTo <- DaVinci.getTarget edge
         return (DaVinciArcType edgeType,
            DaVinciNode nodeFrom,DaVinciNode nodeTo,dyn)

instance DeleteArc DaVinciGraph DaVinciArc where
   deleteArc (DaVinciGraph _ _ arcLabelMVar) (DaVinciArc edge) =
      do
         arcLabelMap <- takeMVar arcLabelMVar
         destroy edge
         putMVar arcLabelMVar (delFromFM arcLabelMap edge)

instance Arc DaVinciArc where

instance ArcType DaVinciArcType where

instance NewArcType DaVinciGraph DaVinciArcType DaVinciArcTypeParms where
   newArcType (DaVinciGraph graph _ _) 
         (DaVinciArcTypeParms labelOpt configList) =
      do
         edgeType <- DaVinci.newEdgeType graph labelOpt configList
         return (DaVinciArcType edgeType)

instance ArcTypeParms DaVinciArcTypeParms where
   emptyArcTypeParms = DaVinciArcTypeParms Nothing []



   
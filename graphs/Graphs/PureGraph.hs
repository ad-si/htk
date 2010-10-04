{-# LANGUAGE FlexibleInstances #-}

-- | This module implements a simple \"pure\" graph interface, destined
-- to be used for the complex graph operations required by VersionDag.
--
-- We instance 'Show' for debugging purposes.
module Graphs.PureGraph(
   PureGraph(..),
   NodeData(..),
   ArcData(..),

   emptyPureGraph, -- :: Ord nodeInfo => PureGraph nodeInfo arcInfo

   addNode, -- :: Ord nodeInfo
      -- => PureGraph nodeInfo arcInfo -> nodeInfo -> [(arcInfo,nodeInfo)]
      -- -> PureGraph nodeInfo arcInfo

   deleteNode, -- :: Ord nodeInfo
      -- => PureGraph nodeInfo arcInfo -> nodeInfo
      -- -> PureGraph nodeInfo arcInfo

   mapArcInfo,
      -- :: (arcInfo1 -> arcInfo2) -> PureGraph nodeInfo arcInfo1
      -- -> PureGraph nodeInfo arcInfo2

   parentNodes,
      -- :: NodeData nodeInfo arcInfo -> [nodeInfo]



   toAllNodes, -- :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> [nodeInfo]
   toNodeParents,
      -- :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo
      -- -> Maybe [nodeInfo]
      -- returns Nothing if the node does not exist.

   nodeExists, -- :: PureGraph nodeInfo arcInfo -> nodeInfo -> Bool

   ) where

import qualified Data.Map as Map

import Graphs.Graph(PartialShow(..))

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

-- | node given with their parent nodes.  The parents should always come
-- before their children in the list.
newtype PureGraph nodeInfo arcInfo = PureGraph {
   nodeDataFM :: Map.Map nodeInfo (NodeData nodeInfo arcInfo)
   }

data NodeData nodeInfo arcInfo = NodeData {
   parents :: [ArcData nodeInfo arcInfo]
   } deriving (Show,Eq,Ord)

data ArcData nodeInfo arcInfo = ArcData {
   arcInfo :: arcInfo,
   target :: nodeInfo
   } deriving (Show,Eq,Ord)

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

-- The Show instances are mainly there for debugging purposes.
instance (Show nodeInfo,Show arcInfo)
      => Show (PureGraph nodeInfo arcInfo) where
   show (PureGraph fm) = show (Map.toList fm)

instance Show (PartialShow (PureGraph nodeInfo arcInfo)) where
   show (PartialShow (PureGraph fm)) = "NParents dump :"
      ++ show (PartialShow (Map.elems fm))

instance Show (PartialShow (NodeData nodeInfo arcInfo)) where
   show (PartialShow nodeData) = "#"++show (length (parents nodeData))

-- ---------------------------------------------------------------------------
-- Creating and modifying graphs
-- ---------------------------------------------------------------------------

emptyPureGraph :: Ord nodeInfo => PureGraph nodeInfo arcInfo
emptyPureGraph = PureGraph Map.empty

-- | add a node with given parent arcs from it.
addNode :: Ord nodeInfo
   => PureGraph nodeInfo arcInfo -> nodeInfo -> [(arcInfo,nodeInfo)]
   -> PureGraph nodeInfo arcInfo
addNode (PureGraph fm) newNode newArcs =
   PureGraph (Map.insert
      newNode
      (NodeData {parents = map
         (\ (arcInfo,target) -> ArcData {arcInfo = arcInfo,target = target})
         newArcs
         }) fm
      )

-- | NB.  The graph will end up ill-formed if you delete a node which
-- has parent arcs pointing to it.
deleteNode :: Ord nodeInfo
   => PureGraph nodeInfo arcInfo -> nodeInfo -> PureGraph nodeInfo arcInfo
deleteNode (PureGraph fm) node = PureGraph (Map.delete node fm)


-- ---------------------------------------------------------------------------
-- Other Elementary functions
-- ---------------------------------------------------------------------------

toAllNodes :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> [nodeInfo]
toAllNodes (PureGraph fm) = Map.keys fm

toNodeParents :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo
   -> Maybe [nodeInfo]
toNodeParents (PureGraph fm) nodeInfo =
   do
      nodeData <- Map.lookup nodeInfo fm
      return (parentNodes nodeData)

nodeExists :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo -> Bool
nodeExists (PureGraph fm) nodeInfo = Map.member nodeInfo fm

mapArcInfo :: (arcInfo1 -> arcInfo2) -> PureGraph nodeInfo arcInfo1
   -> PureGraph nodeInfo arcInfo2
mapArcInfo mapArc (PureGraph fm) =
   PureGraph (Map.mapWithKey
      (\ _ nodeData1 ->
         let
            parents1 = parents nodeData1
            parents2 = map
               (\ arcData1 -> arcData1 {arcInfo = mapArc (arcInfo arcData1)})
               parents1
         in
            nodeData1 {parents = parents2}
         )
      fm
      )

parentNodes :: NodeData nodeInfo arcInfo -> [nodeInfo]
parentNodes nodeData = fmap target (parents nodeData)

-- | This module implements a simple \"pure\" graph interface, destined
-- to be used for the complex graph operations required by VersionDag.
-- 
-- We instance 'Show' for debugging purposes.
module PureGraph(
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

import Data.FiniteMap

import Graph(PartialShow(..))

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

-- | node given with their parent nodes.  The parents should always come
-- before their children in the list.
newtype PureGraph nodeInfo arcInfo = PureGraph {
   nodeDataFM :: FiniteMap nodeInfo (NodeData nodeInfo arcInfo)
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
   show (PureGraph fm) = show (fmToList fm)

instance Show (PartialShow (PureGraph nodeInfo arcInfo)) where
   show (PartialShow (PureGraph fm)) = "NParents dump :" 
      ++ show (PartialShow (eltsFM fm))

instance Show (PartialShow (NodeData nodeInfo arcInfo)) where
   show (PartialShow nodeData) = "#"++show (length (parents nodeData))

-- ---------------------------------------------------------------------------
-- Creating and modifying graphs
-- ---------------------------------------------------------------------------

emptyPureGraph :: Ord nodeInfo => PureGraph nodeInfo arcInfo
emptyPureGraph = PureGraph emptyFM

-- | add a node with given parent arcs from it.
addNode :: Ord nodeInfo 
   => PureGraph nodeInfo arcInfo -> nodeInfo -> [(arcInfo,nodeInfo)] 
   -> PureGraph nodeInfo arcInfo
addNode (PureGraph fm) newNode newArcs =
   PureGraph (addToFM
      fm
      newNode
      (NodeData {parents = map
         (\ (arcInfo,target) -> ArcData {arcInfo = arcInfo,target = target})
         newArcs
         })
      )

-- | NB.  The graph will end up ill-formed if you delete a node which
-- has parent arcs pointing to it.
deleteNode :: Ord nodeInfo
   => PureGraph nodeInfo arcInfo -> nodeInfo -> PureGraph nodeInfo arcInfo
deleteNode (PureGraph fm) node = PureGraph (delFromFM fm node)


-- ---------------------------------------------------------------------------
-- Other Elementary functions
-- ---------------------------------------------------------------------------

toAllNodes :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> [nodeInfo]
toAllNodes (PureGraph fm) = keysFM fm

toNodeParents :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo 
   -> Maybe [nodeInfo]
toNodeParents (PureGraph fm) nodeInfo =
   do
      nodeData <- lookupFM fm nodeInfo
      return (parentNodes nodeData)

nodeExists :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo -> Bool
nodeExists (PureGraph fm) nodeInfo = elemFM nodeInfo fm

mapArcInfo :: (arcInfo1 -> arcInfo2) -> PureGraph nodeInfo arcInfo1 
   -> PureGraph nodeInfo arcInfo2
mapArcInfo mapArc (PureGraph fm) =
   PureGraph (mapFM 
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

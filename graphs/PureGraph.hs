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

   mapArcInfo,
      -- :: (arcInfo1 -> arcInfo2) -> PureGraph nodeInfo arcInfo1 
      -- -> PureGraph nodeInfo arcInfo2

   parentNodes,
      -- :: NodeData nodeInfo arcInfo -> [nodeInfo]

   ) where

import Data.FiniteMap

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
   } deriving (Show) 

data ArcData nodeInfo arcInfo = ArcData {
   arcInfo :: arcInfo,
   target :: nodeInfo
   } deriving (Show,Eq,Ord)

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

-- The Show instance is mainly there for debugging purposes.
instance (Show nodeInfo,Show arcInfo)  
      => Show (PureGraph nodeInfo arcInfo) where
   show (PureGraph fm) = show (fmToList fm)

-- ---------------------------------------------------------------------------
-- Creating and modifying graphs
-- ---------------------------------------------------------------------------

emptyPureGraph :: Ord nodeInfo => PureGraph nodeInfo arcInfo
emptyPureGraph = PureGraph emptyFM

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

-- ---------------------------------------------------------------------------
-- Other Elementary functions
-- ---------------------------------------------------------------------------


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

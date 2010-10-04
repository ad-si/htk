{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The functions in this module implement pruning of 'PureGraph's,
-- to remove hidden nodes as far as possible, while still showing the
-- structure between non-hidden nodes.
--
-- NB.  It is assumed the PureGraph is acyclic!
module Graphs.PureGraphPrune(
   pureGraphPrune,
   ) where


import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Util.ExtendedPrelude

import Graphs.PureGraph


-- | Remove "hidden" vertices as far as possible from a graph, which
-- must be acyclic, while still preserving the structure as far as possible.
pureGraphPrune ::
   (Ord nodeInfo,Ord arcInfo)
   => (nodeInfo -> Bool) -- ^ This function returns True if a node is hidden.
   -> PureGraph nodeInfo arcInfo
   -> PureGraph nodeInfo (Maybe arcInfo)
   -- ^ In the returned graph, we use 'Nothing' to indicate the arcs
   -- which don't correspond to arcs in the original graph.
pureGraphPrune isHidden (pureGraph0 :: PureGraph nodeInfo arcInfo) =
   let
      pureGraph1 :: PureGraph nodeInfo (Maybe arcInfo)
      pureGraph1 = mapArcInfo Just pureGraph0

      pureGraph2 :: PureGraph nodeInfo (Maybe arcInfo)
      pureGraph2 = zTrans isHidden pureGraph1

      pureGraph3 :: PureGraph nodeInfo (Maybe arcInfo)
      pureGraph3 = findNotHanging isHidden pureGraph2

      pureGraph4 :: PureGraph nodeInfo (Maybe arcInfo)
      pureGraph4 = removeOneHiddenParent isHidden pureGraph3
   in
      pureGraph4


-- | Computes list in which parents always precede their children.
orderGraph :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> [nodeInfo]
orderGraph ((PureGraph fm) :: PureGraph nodeInfo arcInfo) =
      reverse (snd (foldl visit (Set.empty,[]) (Map.keys fm)))
   where
      visit :: (Set nodeInfo,[nodeInfo]) -> nodeInfo
         -> (Set nodeInfo,[nodeInfo])
      visit (sl0 @ (set0,list0)) a =
         if Set.member a set0
            then
               sl0
            else
               let
                  nodeData :: NodeData nodeInfo arcInfo
                  Just nodeData = Map.lookup a fm

                  set1 = Set.insert a set0

                  (set2,list1) = foldl visit (set1,list0)
                     (parentNodes nodeData)
               in
                  (set2,a:list1)

-- | Transform the Dag according to the Z function.
-- The rule is that hidden nodes with just one parent get replaced
-- in parent lists by their parent (repeatedly).
zTrans :: (Ord nodeInfo,Ord arcInfo)
   => (nodeInfo -> Bool)
   -> PureGraph nodeInfo (Maybe arcInfo)
   -> PureGraph nodeInfo (Maybe arcInfo)
zTrans isHidden ((pureGraph @ (PureGraph fm))
      :: PureGraph nodeInfo (Maybe arcInfo)) =
   let
      ordered = orderGraph pureGraph

      compute ::
          Map.Map nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
          -> nodeInfo
          -> Map.Map nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
      compute z0 (a :: nodeInfo) =
         let
            nodeData :: NodeData nodeInfo (Maybe arcInfo)
            Just nodeData = Map.lookup a fm

            mapParent ::
               ArcData nodeInfo (Maybe arcInfo)
               -> ArcData nodeInfo (Maybe arcInfo)
            mapParent arcData = case Map.lookup (target arcData) z0 of
               Just (parentNode,_) | parentNode /= target arcData
                  -> newArc parentNode
               _ -> arcData

            parents1 = uniqOrd (fmap mapParent (parents nodeData))

            za =
               if isHidden a
                  then
                     case parents1 of
                        [parent1] -> target parent1
                        _ -> a
                  else
                     a
         in
            Map.insert a (za,NodeData {
               parents = parents1
               }) z0

      zMap :: Map.Map nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
      zMap = foldl compute Map.empty ordered

      fm2 :: Map.Map nodeInfo (NodeData nodeInfo (Maybe arcInfo))
      fm2 = Map.mapWithKey
         (\ a (_,nodeData) -> nodeData)
         zMap
   in
      PureGraph fm2

-- | Compute all nodes which are either not hidden, or have a descendant
-- which is not hidden, and then delete all other nodes.
findNotHanging :: Ord nodeInfo
   => (nodeInfo -> Bool)
   -> PureGraph nodeInfo (Maybe arcInfo)
   -> PureGraph nodeInfo (Maybe arcInfo)
findNotHanging isHidden (PureGraph fm :: PureGraph nodeInfo (Maybe arcInfo)) =
   let
      visit :: Set nodeInfo -> nodeInfo -> Set nodeInfo
      visit set0 a =
         let
            set1 = Set.insert a set0
            Just nodeData = Map.lookup a fm
         in
            visits set1 (parentNodes nodeData)

      visits :: Set nodeInfo -> [nodeInfo] -> Set nodeInfo
      visits set0 as = foldl visit set0 as

      notHidden :: [nodeInfo]
      notHidden = mapMaybe
         (\ a -> if isHidden a then Nothing else Just a)
         (Map.keys fm)

      notHanging :: Set nodeInfo
      notHanging = visits Set.empty notHidden

      notHangingFM = foldl
         (\ fm0 a ->
            let
               Just nodeData = Map.lookup a fm
            in
               Map.insert a nodeData fm0
            )
         Map.empty
         (Set.toList notHanging)
   in
      PureGraph notHangingFM

-- | Compute the number of children each node has in a Dag
nChildren :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> nodeInfo -> Int
nChildren (PureGraph fm :: PureGraph nodeInfo arcInfo) nf =
   let
      fm1 = Map.foldWithKey
         (\ a nodeData fm0 ->
            let
               parents1 = parentNodes nodeData
            in
               foldl
                  (\ fm0 parent ->
                     Map.insert parent (Map.findWithDefault 0 parent fm0 + 1)
                     fm0
                     )
                  fm0
                  parents1
            )
         (Map.empty :: Map.Map nodeInfo Int)
         fm
   in
      Map.findWithDefault 0 nf fm1

-- | For nodes with one hidden parent, which has just that child,
-- delete the hidden parent and replace the original node's parents by the
-- hidden parent's parents.
--
-- NB.  We don't have to worry about this being applied recursively provided
-- zTrans has already been applied, since that removes chains of hidden
-- vertices.
removeOneHiddenParent :: forall nodeInfo arcInfo . Ord nodeInfo
   => (nodeInfo -> Bool)
   -> PureGraph nodeInfo (Maybe arcInfo)
   -> PureGraph nodeInfo (Maybe arcInfo)
removeOneHiddenParent isHidden (pureGraph @ (PureGraph fm0)
      ::  PureGraph nodeInfo (Maybe arcInfo)) =
   let
      nc = nChildren pureGraph

      candidates0 :: [(nodeInfo,NodeData nodeInfo (Maybe arcInfo))]
      candidates0 = Map.toList fm0

      deletions :: [(nodeInfo,nodeInfo,NodeData nodeInfo (Maybe arcInfo))]
      deletions = mapMaybe
         (\ (a,nodeData) -> case parentNodes nodeData of
           [parent] ->
              if nc parent == 1
                 then
                    case Map.lookup parent fm0 of
                       Just nodeData | isHidden parent ->
                          let
                             parentNodes1 = parentNodes nodeData
                             parents1 = fmap newArc parentNodes1
                          in
                             Just (a,parent,NodeData {parents = parents1})
                       _ -> Nothing
                 else
                    Nothing
           _ -> Nothing
           )
        candidates0

      fm1 = foldl
         (\ fm0 (a,parent,nodeData) ->
            (Map.insert a nodeData (Map.delete parent fm0))
            )
         fm0
         deletions
   in
       PureGraph fm1

newArc :: nodeInfo -> ArcData nodeInfo (Maybe arcInfo)
newArc nodeInfo = ArcData {target = nodeInfo,arcInfo = Nothing}

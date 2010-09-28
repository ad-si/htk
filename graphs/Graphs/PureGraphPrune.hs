-- | The functions in this module implement pruning of 'PureGraph's,
-- to remove hidden nodes as far as possible, while still showing the
-- structure between non-hidden nodes.
--
-- NB.  It is assumed the PureGraph is acyclic!
module Graphs.PureGraphPrune(
   pureGraphPrune,
   ) where


import Maybe

import Util.DeprecatedFiniteMap
import Util.DeprecatedSet

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
      reverse (snd (foldl visit (emptySet,[]) (keysFM fm)))
   where
      visit :: (Set nodeInfo,[nodeInfo]) -> nodeInfo
         -> (Set nodeInfo,[nodeInfo])
      visit (sl0 @ (set0,list0)) a =
         if elementOf a set0
            then
               sl0
            else
               let
                  nodeData :: NodeData nodeInfo arcInfo
                  Just nodeData = lookupFM fm a

                  set1 = addToSet set0 a

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
          FiniteMap nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
          -> nodeInfo
          -> FiniteMap nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
      compute z0 (a :: nodeInfo) =
         let
            nodeData :: NodeData nodeInfo (Maybe arcInfo)
            Just nodeData = lookupFM fm a

            mapParent ::
               ArcData nodeInfo (Maybe arcInfo)
               -> ArcData nodeInfo (Maybe arcInfo)
            mapParent arcData = case lookupFM z0 (target arcData) of
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
            addToFM z0 a (za,NodeData {
               parents = parents1
               })

      zMap :: FiniteMap nodeInfo (nodeInfo,NodeData nodeInfo (Maybe arcInfo))
      zMap = foldl compute emptyFM ordered

      fm2 :: FiniteMap nodeInfo (NodeData nodeInfo (Maybe arcInfo))
      fm2 = mapFM
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
            set1 = addToSet set0 a
            Just nodeData = lookupFM fm a
         in
            visits set1 (parentNodes nodeData)

      visits :: Set nodeInfo -> [nodeInfo] -> Set nodeInfo
      visits set0 as = foldl visit set0 as

      notHidden :: [nodeInfo]
      notHidden = mapMaybe
         (\ a -> if isHidden a then Nothing else Just a)
         (keysFM fm)

      notHanging :: Set nodeInfo
      notHanging = visits emptySet notHidden

      notHangingFM = foldl
         (\ fm0 a ->
            let
               Just nodeData = lookupFM fm a
            in
               addToFM fm0 a nodeData
            )
         emptyFM
         (setToList notHanging)
   in
      PureGraph notHangingFM

-- | Compute the number of children each node has in a Dag
nChildren :: Ord nodeInfo => PureGraph nodeInfo arcInfo -> (nodeInfo -> Int)
nChildren (PureGraph fm :: PureGraph nodeInfo arcInfo) =
   let
      fm1 = foldFM
         (\ a nodeData fm0 ->
            let
               parents1 = parentNodes nodeData
            in
               foldl
                  (\ fm0 parent ->
                     addToFM fm0 parent (lookupWithDefaultFM fm0 0 parent + 1)
                     )
                  fm0
                  parents1
            )
         (emptyFM :: FiniteMap nodeInfo Int)
         fm
   in
      lookupWithDefaultFM fm1 0

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
      candidates0 = fmToList fm0

      deletions :: [(nodeInfo,nodeInfo,NodeData nodeInfo (Maybe arcInfo))]
      deletions = mapMaybe
         (\ (a,nodeData) -> case parentNodes nodeData of
           [parent] ->
              if nc parent == 1
                 then
                    case lookupFM fm0 parent of
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
            (addToFM (delFromFM fm0 parent) a nodeData)
            )
         fm0
         deletions
   in
       PureGraph fm1

newArc :: nodeInfo -> ArcData nodeInfo (Maybe arcInfo)
newArc nodeInfo = ArcData {target = nodeInfo,arcInfo = Nothing}

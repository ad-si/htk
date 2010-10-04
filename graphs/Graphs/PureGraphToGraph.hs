{-# LANGUAGE ScopedTypeVariables #-}

-- | This module, given a changing source of 'PureGraph's, transforms it into
-- a 'Graph'. -}
module Graphs.PureGraphToGraph(
   pureGraphToGraph,
   ) where

import Data.List

import qualified Data.Map as Map
import Data.IORef

import Util.Computation(done)
import Util.Sources
import Util.Sink
import Util.AtomString
import Util.ExtendedPrelude

import Graphs.Graph
import Graphs.NewNames
import Graphs.PureGraph


-- ------------------------------------------------------------------------
-- Data types
-- ------------------------------------------------------------------------

data State nodeKey nodeInfo arcInfo = State {
   nameSource :: NameSource,
      -- ^ source of new names
   pureGraph :: PureGraph (nodeKey,Node) (arcInfo,Arc),
      -- ^ current annotated graph
   toNodeInfo :: nodeKey -> nodeInfo
      -- ^ current node info
   }

-- ------------------------------------------------------------------------
-- Functions
-- ------------------------------------------------------------------------

pureGraphToGraph :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => SimpleSource (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
   -> GraphConnection nodeInfo () arcInfo ()
pureGraphToGraph (simpleSource
      :: SimpleSource (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)) =
   let
      source1 ::
         Source (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
                (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
      source1 = toSource simpleSource

      source2 ::
         Source (State nodeKey nodeInfo arcInfo,
               CannedGraph nodeInfo () arcInfo ())
            [Update nodeInfo () arcInfo ()]
      source2 = foldSourceIO getStateFn foldStateFn source1

      source3 ::
         Source (State nodeKey nodeInfo arcInfo,
               CannedGraph nodeInfo () arcInfo ())
            (Update nodeInfo () arcInfo ())
      source3 = map2 MultiUpdate source2

      addConnection doUpdate =
         do
            ((state,cannedGraph),sink)<- addNewSink source3 doUpdate
            nameSourceBranch <- branch (nameSource state)

            let
               graphConnectionData = GraphConnectionData {
                  graphState = cannedGraph,
                  deRegister = invalidate sink,
                  graphUpdate = (\ update -> done),
                     -- updates from the client are ignored
                  nameSourceBranch = nameSourceBranch
                  }

            return graphConnectionData
   in
      addConnection


getStateFn
   :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
   -> IO (State nodeKey nodeInfo arcInfo,CannedGraph nodeInfo () arcInfo ())
getStateFn (pureGraph0,toNodeInfo0) =
   do
      nameSource <- useBranch initialBranch

      (pureGraph1,updates0)
         <- modifyPureGraph nameSource emptyPureGraph pureGraph0
            (error "PureGraphToGraph: no old nodes") toNodeInfo0
      let
         state = State {
            nameSource = nameSource,
            pureGraph = pureGraph1,
            toNodeInfo = toNodeInfo0
            }

         updates1 = typeUpdates ++ updates0

         cannedGraph = CannedGraph {updates = updates1}

      return (state,cannedGraph)

foldStateFn
   :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => State nodeKey nodeInfo arcInfo
   -> (PureGraph nodeKey arcInfo,nodeKey -> nodeInfo)
   -> IO (State nodeKey nodeInfo arcInfo,[Update nodeInfo () arcInfo ()])
foldStateFn state (pureGraph0,toNodeInfo1) =
   do
      (pureGraph1,updates)
         <- modifyPureGraph (nameSource state) (pureGraph state) pureGraph0
            (toNodeInfo state) toNodeInfo1
      return (state {pureGraph = pureGraph1,toNodeInfo = toNodeInfo1},updates)


modifyPureGraph :: (Ord nodeKey,Ord arcInfo,Eq nodeInfo)
   => NameSource
      -- ^ How we generate new Node and Arc values
   -> PureGraph (nodeKey,Node) (arcInfo,Arc)
      -- ^ the old graph, annotated with corresponding node and arc values
   -> PureGraph nodeKey arcInfo
      -- ^ the new graph
   -> (nodeKey -> nodeInfo)
      -- ^ old toNodeInfo function
   -> (nodeKey -> nodeInfo)
      -- ^ new toNodeInfo function
   -> IO (PureGraph (nodeKey,Node) (arcInfo,Arc),
      [Update nodeInfo () arcInfo ()])
      -- ^ the new annotated graph, and the changes to get to it.
modifyPureGraph nameSource
      (pg @ (PureGraph oldFM0 :: PureGraph (nodeKey,Node) (arcInfo,Arc)))
      (PureGraph newFM0 :: PureGraph nodeKey arcInfo)
      (oldToNodeInfo :: nodeKey -> nodeInfo)
      (toNodeInfo :: nodeKey -> nodeInfo) =
   do
      -- Node-generating mechanism.  We generate nodes dynamically as we
      -- look them up.
      (nodeIORef :: IORef (Map.Map nodeKey Node)) <- newIORef Map.empty
      let
         lookupNode :: nodeKey -> IO Node
         lookupNode nodeKey = case lookupPureNode pg nodeKey of
            Just node -> return node
            Nothing ->
               do
                  fm <- readIORef nodeIORef
                  case Map.lookup nodeKey fm of
                     Just node -> return node
                     Nothing ->
                        do
                           nodeStr <- getNewName nameSource
                           let
                              node = fromString nodeStr
                           writeIORef nodeIORef (Map.insert nodeKey node fm)
                           return node


         oldFM0List :: [((nodeKey,Node),
            NodeData (nodeKey,Node) (arcInfo,Arc))]
         oldFM0List = Map.toList oldFM0

         newFM0List :: [(nodeKey,NodeData nodeKey arcInfo)]
         newFM0List = Map.toList newFM0

         -- type arguments for generalisedMerge

         -- a :: ((nodeKey,Node),NodeData (nodeKey,Node)
         --       (arcInfo,Arc))
         -- b :: (nodeKey,NodeData nodeKey arcInfo)
         -- c :: [Update nodeInfo () arcInfo ()]
         toKey1 :: ((nodeKey,Node),NodeData (nodeKey,Node)
            (arcInfo,Arc)) -> nodeKey
         toKey1 = fst . fst

         toKey2 :: (nodeKey,NodeData nodeKey arcInfo) -> nodeKey
         toKey2 = fst

         compareFn a b = compare (toKey1 a) (toKey2 b)

         mergeFn ::
            Maybe ((nodeKey,Node),
               NodeData (nodeKey,Node) (arcInfo,Arc))
            -> Maybe (nodeKey,NodeData nodeKey arcInfo)
            -> IO (Maybe ((nodeKey,Node),
                  NodeData (nodeKey,Node)
               (arcInfo,Arc)),Maybe [Update nodeInfo () arcInfo ()])
         mergeFn (Just ((nodeKey,node),nodeData)) Nothing =
            -- this node must be deleted
            do
               let
                  update1 = DeleteNode node
               ([],updates) <- modifyArcs (parents nodeData) []
                  node nameSource lookupNode
               return (Nothing,Just (update1:updates))
         mergeFn Nothing (Just (nodeKey,nodeData)) =
            -- this node must be added
            do
               node <- lookupNode nodeKey
               let
                  nodeInfo = toNodeInfo nodeKey
                  update1 = NewNode node theNodeType nodeInfo
               (arcDatas,updates) <- modifyArcs [] (parents nodeData)
                  node nameSource lookupNode
               return (Just ((nodeKey,node),
                  NodeData {parents = arcDatas}),
                     Just (update1:updates))
         mergeFn (Just (nn @(nodeKey1,node),nodeData1))
               (Just (nodeKey2,nodeData2)) =
            -- node needs to be neither added nor deleted, but the NodeData
            -- might have changed and we might need to change the nodeData
            do
               (arcDatas,updates1) <- modifyArcs (parents nodeData1)
                  (parents nodeData2)
                  node nameSource lookupNode
               let
                  nodeInfo1 = oldToNodeInfo nodeKey1
                  nodeInfo2 = toNodeInfo nodeKey2

                  updates2 = if nodeInfo1 == nodeInfo2
                     then
                        []
                     else [SetNodeLabel node nodeInfo2]

                  updates = updates1 ++ updates2

               return (Just (nn,NodeData {parents = arcDatas}),Just updates)

      (newFM1List,updatess0)
         <- generalisedMerge oldFM0List newFM0List compareFn mergeFn

      -- To make the updates consistent, sort them into the order
      -- (delete arcs) (delete nodes) (add nodes) (set node labels) (add arcs)
      let
         pg1 = PureGraph (Map.fromList newFM1List)


         updates0 = concat updatess0

         updates1 =
               [ update | (update @ (DeleteArc _ )) <- updates0 ]
            ++ [ update | (update @ (DeleteNode _ )) <- updates0 ]
            ++ [ update | (update @ (NewNode _ _ _ )) <- updates0 ]
            ++ [ update | (update @ (NewArc _ _ _ _ _ )) <- updates0 ]
            ++ [ update | (update @ (SetNodeLabel _ _)) <- updates0 ]

      return (pg1,updates1)

lookupPureNode :: Ord nodeKey
   => PureGraph (nodeKey,Node) (arcInfo,arc)
   -> nodeKey
   -> Maybe Node
lookupPureNode (PureGraph fm) nodeKey0 =
  case filter (\ ((nodeKey1, _), _) -> nodeKey1 == nodeKey0) $ Map.toList fm of
      ((_,node),_) : _  -> Just node
      _ -> Nothing

modifyArcs :: (Ord nodeKey,Ord arcInfo)
   -- Invariant.  fromArcs should only be generated by modifyArcs or
   -- else [].  This means we can assume it is sorted.

   => [ArcData (nodeKey,Node) (arcInfo,Arc)]
   -> [ArcData nodeKey arcInfo]
   -> Node -> NameSource -> (nodeKey -> IO Node)
   -> IO ([ArcData (nodeKey,Node) (arcInfo,Arc)],
      [Update nodeInfo () arcInfo ()])
modifyArcs (fromArcs :: [ArcData (nodeKey,Node) (arcInfo,Arc)]) ontoArcs0
      sourceNode nameSource lookupNode =
   let
      toKey :: ArcData (nodeKey,Node) (arcInfo,Arc) ->
         ArcData nodeKey arcInfo
      toKey arcData0 = ArcData {
         arcInfo = fst . arcInfo $ arcData0,
         target = fst . target $ arcData0
         }

      -- (1) sort ontoArcs.  (fromArcs should already be sorted)
      ontoArcs1 = sort ontoArcs0

      -- (2) define functions for generalisedMerge
      compareFn :: ArcData (nodeKey,Node) (arcInfo,Arc)
         -> ArcData nodeKey arcInfo -> Ordering
      compareFn arc1 arc2 = compare (toKey arc1) arc2

      mergeFn :: Maybe (ArcData (nodeKey,Node) (arcInfo,Arc))
         -> Maybe (ArcData nodeKey arcInfo)
         -> IO (Maybe (ArcData (nodeKey,Node) (arcInfo,Arc)),
            Maybe (Update nodeInfo () arcInfo ()))
      mergeFn (Just arcData) Nothing =
         return (Nothing,Just (DeleteArc (snd . arcInfo $ arcData)))
      mergeFn Nothing (Just arcData0) =
         do
            arcStr <- getNewName nameSource
            let
               arc :: Arc
               arc = fromString arcStr

            (targetNode :: Node) <- lookupNode (target arcData0)

            let
               arcInfo1 = arcInfo arcData0

               arcData1 = ArcData {
                  arcInfo = (arcInfo1,arc),
                  target = (target arcData0,targetNode)
                  }
            return (Just arcData1,Just
               (NewArc arc theArcType arcInfo1 targetNode sourceNode))
      mergeFn (Just arcData1) (Just _) = return (Just arcData1,Nothing)
   in
      generalisedMerge fromArcs ontoArcs1 compareFn mergeFn

-- ----------------------------------------------------------------------
-- Node and Arc types
-- We only have one of each.
-- ----------------------------------------------------------------------

theNodeType :: NodeType
theNodeType = fromString ""

theArcType :: ArcType
theArcType = fromString ""

typeUpdates :: [Update nodeInfo () arcInfo ()]
typeUpdates = [NewNodeType theNodeType (),NewArcType theArcType ()]

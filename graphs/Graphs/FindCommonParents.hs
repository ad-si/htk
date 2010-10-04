{-# LANGUAGE ScopedTypeVariables #-}

-- | Given two acyclic graphs G1 and G2 sharing some nodes, and a list V1 of nodes in G1,
-- let A be the union of G1 intersect G2 and V1.  The function in this module returns
-- a list L of type [(Node,[Node])] such that
-- (1) The first elements in each pair in L are precisely those elements of V1 not in G2.
-- (2) For each element (n,ms) in L,
--     the list ms contains precisely those vertices m of G1 such that
--     (a) m is in A;
--     (b) there is a path from m to n in G1 which has no common vertices with
--         A except at its endpoints.
-- (3) Where the list contains two elements (n1,ms1) and (n2,ms2), such that
--     ms2 contains n1, then (n1,ms1) comes before (n2,ms2) in the list.
--
-- The purpose of all this is to provide a list of the nodes to be constructed
-- in G2 to extend it by V1 while preserving as much as possible of the path
-- structure in V1.  This is used for adding version graph information.
module Graphs.FindCommonParents(
   findCommonParents,
   GraphBack(..),
   ) where

import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphs.TopSort

-- ----------------------------------------------------------------------------
-- GraphBack
-- encoded information about a graph needed for this operation
--
-- NB.  GraphBack is now used for other purposes in other modules.
-- ----------------------------------------------------------------------------

data GraphBack node nodeKey = GraphBack {
   getAllNodes :: [node],
      -- ^ Get all nodes in the graph
   getKey :: node -> (Maybe nodeKey),
      -- ^ If the node does not exist in the graph 'Nothing'.
      -- Otherwise 'Just' key where key is a \"nodeKey\", an ordered key
      -- uniquely distinguishing the node (and used to detect common elements
      -- in the two graphs)
   getParents :: node -> (Maybe [node])
      -- ^If node does not exist Nothing, otherwise immediate
      -- parents of node.
   }

-- ----------------------------------------------------------------------------
-- The function
-- ----------------------------------------------------------------------------

findCommonParents :: (Show node1,Show node2,Show nodeKey,Ord nodeKey)
   => GraphBack node1 nodeKey -> GraphBack node2 nodeKey -> [node1]
   -> [(node1,[(node1,Maybe node2)])]
   -- G1, G2 and V1.
   -- Note that the nodes are kept distinct, even by type; they can only be
   --    compared by nodeKey.
   -- The returned list [(node1,Maybe node2)] contains the parents of
   -- the node, each element corresponding to one parent, with first
   -- the node in the first graph, and second (if it already exists) the
   -- node in the second graph.
findCommonParents
      (g1 :: GraphBack node1 nodeKey) (g2 :: GraphBack node2 nodeKey)
      (v1 :: [node1]) =
   let
      getKey1 = getKey g1
      getKey2 = getKey g2

      getParents1 = getParents g1

      -- (1) construct dictionaries by NodeKey for all nodes in g2 and v1.
      v1Dict :: Map.Map nodeKey node1
      v1Dict =
         foldl
            (\ map0 v1Node ->
               let
                  Just nodeKey = getKey1 v1Node
                     -- Nothing here indicates an element of v1 not in G1.
               in
                  Map.insert nodeKey v1Node map0
               )
            Map.empty
            v1

      g2Nodes :: [node2]
      g2Nodes = getAllNodes g2

      g2Dict :: Map.Map nodeKey node2
      g2Dict =
         foldl
            (\ map0 g2Node ->
              let
                 Just nodeKey = getKey2 g2Node
                    -- Nothing here indicates an element of g2Nodes not in g2.
              in
                 Map.insert nodeKey g2Node map0
              )
           Map.empty
           g2Nodes

      -- doNode gets the list for the given node, or Nothing if it is
      -- already in G2.
      doNode :: node1 -> Maybe [(node1,Maybe node2)]
      doNode node =
         let
            Just nodeKey = getKey1 node
         in
            case Map.lookup nodeKey g2Dict of
               Just _ -> Nothing -- already is G2.
               Nothing ->
                  let
                     Just nodes = getParents1 node
                     (_,list) = doNodes nodes Set.empty []
                  in
                     Just (reverse list)
         where
            --
            -- The following functions have the job of scanning back
            -- through g1, looking for parents also in g2, or which
            -- will be by merit of being copied.
            doNodes :: [node1] -> Set.Set nodeKey -> [(node1,Maybe node2)]
               -> (Set.Set nodeKey,[(node1,Maybe node2)])
            -- Set is visited set.
            -- list is accumulating parameter.
            doNodes nodes visited0 acc0 =
               foldl
                  (\ (visited0,acc0) node -> doNode1 node visited0 acc0)
                  (visited0,acc0)
                  nodes

            doNode1 :: node1 -> Set.Set nodeKey -> [(node1,Maybe node2)]
               -> (Set.Set nodeKey,[(node1,Maybe node2)])
            -- Set is visited set, ancestors already visited.
            -- list is accumulating parameter.
            doNode1 node1 visited0 acc0 =
               -- Examine node1 to see if it is common ancestor.
               let
                  Just nodeKey = getKey1 node1
               in
                  if Set.member nodeKey visited0
                     then
                        (visited0,acc0)
                     else
                        let
                           visited1 = Set.insert nodeKey visited0
                        in
                           case (Map.lookup nodeKey g2Dict,
                                 Map.lookup nodeKey v1Dict) of
                              (Just node2,_) ->
                                 -- Node is in g2.  Since node was found
                                 -- by scanning back in graph1,
                                 -- it is also in graph1.  Hence this is
                                 -- a common node.
                                 (visited1,(node1,Just node2) : acc0)
                              (Nothing,Just node1) ->
                                 -- This node is in v, but not g2 yet.
                                 (visited1,(node1,Nothing) : acc0)
                              (Nothing,Nothing) ->
                                 -- Have to scan back to this node's
                                 -- ancestors.
                                 let
                                    Just nodes = getParents1 node1
                                 in
                                    doNodes nodes visited1 acc0

      -- (2) Get the list, but don't sort out the order yet.
      nodes1Opt :: [Maybe (node1,[(node1,Maybe node2)])]
      nodes1Opt =
         fmap
            (\ v1Node ->
               let
                  nodesOpt = doNode v1Node
               in
                  (fmap (\ nodes -> (v1Node,nodes)) nodesOpt)
               )
            v1

      nodes1 :: [(node1,[(node1,Maybe node2)])]
      nodes1 = catMaybes nodes1Opt

      -- (3) Construct a map from nodeKey to the elements of this list.
      nodeKeyMap :: Map.Map nodeKey (node1,[(node1,Maybe node2)])
      nodeKeyMap = foldl
         (\ map0 (nodeData @ (node1,nodes)) ->
            let
               Just nodeKey = getKey1 node1
            in
               Map.insert nodeKey nodeData map0
            )
         Map.empty
         nodes1

      -- (4) transform nodes1 list into an list of relations
      -- [(nodeKey,nodeKey)], ready to feed to TopSort.topSort.  Hence the key
      -- that needs to come first in the result -- the ancestor --
      -- needs to go first in the pair.
      relations1 :: [(nodeKey,[nodeKey])]
      relations1 =
         fmap
            (\ (node,nodes) ->
               let
                  Just nodeKey = getKey1 node

                  nodeKeysOpt :: [Maybe nodeKey]
                  nodeKeysOpt = fmap
                     (\ nodeItem -> case nodeItem of
                        (node1,Nothing) ->
                           let
                              Just nodeKey2 = getKey1 node1
                           in
                              Just nodeKey2
                        (node1,Just _) -> Nothing
                        )
                     nodes
               in
                  (nodeKey,catMaybes nodeKeysOpt)
               )
            nodes1

      relations :: [(nodeKey,nodeKey)]
      relations = concat
         (fmap
             (\ (thisNodeKey,nodeKeys) ->
                fmap
                   (\ parentNodeKey -> (parentNodeKey,thisNodeKey))
                   nodeKeys
                )
             relations1
             )

      nodeKeys :: [nodeKey]
      nodeKeys = fmap (\ (thisNodeKey,_) -> thisNodeKey) relations1

      -- (5) do a topological sort.
      nodeKeysInOrder :: [nodeKey]
      nodeKeysInOrder = topSort1 relations nodeKeys

      -- (6) Put the output together
      nodesOut :: [(node1,[(node1,Maybe node2)])]
      nodesOut =
         fmap
            (\ nodeKey ->
               let
                  Just nodeData = Map.lookup nodeKey nodeKeyMap
               in
                  nodeData
               )
            nodeKeysInOrder
   in
      nodesOut

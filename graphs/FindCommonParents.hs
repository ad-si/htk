{- Given two acyclic graphs G1 and G2 sharing some nodes, and a list V1 of nodes in G1,
   let A be the union of G1 intersect G2 and V1.  The function in this module returns
   a list L of type [(Node,[Node])] such that 
   (1) The first elements in each pair in L are precisely those elements of V1 not in G2.
   (2) For each element (n,ms) in L,
       the list ms contains precisely those vertices m of G1 such that
       (a) m is in A;
       (b) there is a path from m to n in G1 which has no common vertices with A except
           at its endpoints.
   (3) Where the list contains two elements (n1,ms1) and (n2,ms2), such that
       ms2 contains n1, then (n1,ms1) comes before (n2,ms2) in the list.

   We also 

   The purpose of all this is to provide a list of the nodes to be constructed in G2
   to extend it by V1 while preserving as much as possible of the path structure in V1.
   This is used for adding version graph information. -}
module FindCommonParents(
   findCommonParents,
   GraphBack(..),
   ) where

import Maybe
import Monad       

import Data.FiniteMap
import Data.Set

import ExtendedPrelude

import TopSort

-- ----------------------------------------------------------------------------
-- GraphBack
-- encoded information about a graph needed for this operation
-- ----------------------------------------------------------------------------

data GraphBack node nodeKey = GraphBack {
   getAllNodes :: IO [node],
      -- Get all nodes in the graph
   getKey :: node -> IO (Maybe nodeKey),
      -- If the node does not exist in the graph return Nothing.  
      -- If it does return Just key where key is a "nodeKey", an ordered key
      -- uniquely distinguishing the node (and used to detect common elements in
      -- the two graphs)
   getParents :: node -> IO (Maybe [node])
      -- If node does not exist return Nothing, otherwise return immediate parents of node.
   }

-- ----------------------------------------------------------------------------
-- The function
-- ----------------------------------------------------------------------------

findCommonParents :: Ord nodeKey 
   => GraphBack node1 nodeKey -> GraphBack node2 nodeKey -> [node1] 
   -> IO [(node1,[Either node1 node2])]
   -- G1, G2 and V1.
   -- Note that the nodes are kept distinct, even by type; they can only be compared
   --    by nodeKey.  In the returned list Left node1 is an element of v1,
   --    and Right node2 an element of G2.
findCommonParents (g1 :: GraphBack node1 nodeKey) (g2 :: GraphBack node2 nodeKey) 
      (v1 :: [node1]) =
   do
      let
         getKey1 = getKey g1
         getKey2 = getKey g2

         getParents1 = getParents g1

      -- (1) construct dictionaries by NodeKey for all nodes in g2 and v1.
      (v1Dict :: FiniteMap nodeKey node1) <-
         foldM
            (\ map0 v1Node ->
               do
                  Just nodeKey <- getKey1 v1Node
                     -- Nothing here indicates an element of v1 not in G1.
                  return (addToFM map0 nodeKey v1Node)
               )
            emptyFM
            v1

      (g2Nodes :: [node2]) <- getAllNodes g2
      (g2Dict :: FiniteMap nodeKey node2) <-
         foldM
            (\ map0 g2Node ->
              do
                 Just nodeKey <- getKey2 g2Node
                    -- Nothing here indicates an element of g2Nodes not in g2.
                 return (addToFM map0 nodeKey g2Node)
              )
           emptyFM
           g2Nodes

      let
         -- doNode gets the list for the given node, or Nothing if it is already in
         -- G2.
         doNode :: node1 -> IO (Maybe [Either node1 node2])
         doNode node =
            do
               Just nodeKey <- getKey1 node
               case lookupFM g2Dict nodeKey of
                  Just _ -> return Nothing -- already is G2.
                  Nothing ->
                     do
                        Just nodes <- getParents1 node                         
                        (_,list) <- doNodes nodes emptySet []
                        return (Just (reverse list))
            where
               doNodes :: [node1] -> Set nodeKey -> [Either node1 node2]
                  -> IO (Set nodeKey,[Either node1 node2])
               -- Set is visited set.
               -- list is accumulating parameter.
               doNodes nodes visited0 acc0 =
                  foldM
                     (\ (visited0,acc0) node -> doNode1 node visited0 acc0)
                     (visited0,acc0)
                     nodes

               doNode1 :: node1 -> Set nodeKey -> [Either node1 node2] 
                  -> IO (Set nodeKey,[Either node1 node2])
               -- Set is visited set.
               -- list is accumulating parameter.
               doNode1 node visited0 acc0 =
                  do
                     Just nodeKey <- getKey1 node      
                     if elementOf nodeKey visited0
                        then 
                           return (visited0,acc0)
                        else
                           do
                              let
                                 visited1 = addToSet visited0 nodeKey
                              case (lookupFM g2Dict nodeKey,lookupFM v1Dict nodeKey) of
                                 (Just node2,_) -> -- found a node in g2
                                    return (visited1,Right node2 : acc0)
                                 (Nothing,Just node1) -> -- found a node in v1
                                    return (visited1,Left node1 : acc0)
                                 (Nothing,Nothing) ->
                                    do
                                       Just nodes <- getParents1 node
                                       doNodes nodes visited1 acc0

      -- (2) Get the list, but don't sort out the order yet.
      (nodes1Opt :: [Maybe (node1,[Either node1 node2])]) <-
         mapM
            (\ v1Node ->
               do
                  nodesOpt <- doNode v1Node
                  return (fmap (\ nodes -> (v1Node,nodes)) nodesOpt)
               )
            v1
      let
         nodes1 :: [(node1,[Either node1 node2])]
         nodes1 = catMaybes nodes1Opt

      -- (3) Construct a map from nodeKey to the elements of this list. 
      (nodeKeyMap :: FiniteMap nodeKey (node1,[Either node1 node2])) <- foldM
         (\ map0 (nodeData @ (node1,nodes)) ->
            do
               Just nodeKey <- getKey1 node1
               return (addToFM map0 nodeKey nodeData)
            ) 
         emptyFM
         nodes1

      -- (4) transform nodes1 list into an list of relations [(nodeKey,nodeKey)],
      -- ready to feed to TopSort.topSort.  Hence the key that needs to come first in the 
      -- final -- the ancestor -- needs to go first in the pair.
      (relations1 :: [(nodeKey,[nodeKey])]) <-
         mapM
            (\ (node,nodes) ->
               do
                  Just nodeKey <- getKey1 node
                  (nodeKeysOpt :: [Maybe nodeKey]) <- mapM
                     (\ nodeItem -> case nodeItem of
                        Left node1 ->
                           do
                              Just nodeKey2 <- getKey1 node1
                              return (Just nodeKey2)
                        Right _ -> return Nothing
                        )
                     nodes
                  return (nodeKey,catMaybes nodeKeysOpt)
               )  
            nodes1

      let
         relations :: [(nodeKey,nodeKey)]
         relations = concat
            (map
                (\ (thisNodeKey,nodeKeys) ->
                   map
                      (\ parentNodeKey -> (parentNodeKey,thisNodeKey))
                      nodeKeys
                   )
                relations1
                )

         -- (5) do a topological sort.
         nodeKeysInOrder :: [nodeKey]
         nodeKeysInOrder = topSort relations

         -- (6) Put the output together
         nodesOut :: [(node1,[Either node1 node2])]
         nodesOut =
            map
               (\ nodeKey ->
                  let
                     Just nodeData = lookupFM nodeKeyMap nodeKey
                  in
                     nodeData
                  )
               nodeKeysInOrder

      return nodesOut 
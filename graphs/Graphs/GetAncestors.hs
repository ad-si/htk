{-# LANGUAGE ScopedTypeVariables #-}

module Graphs.GetAncestors(
   getAncestors,
      -- :: Graph graph => Bool
      -- -> graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
      --
      -- Given an acyclic graph, a function (f :: nodeLabel -> IO Bool),
      -- and a node v, we return the set of nodes W such that for w in W,
      -- (1) (f w) returns True
      -- (2) there is a path from w to v, such that
      --     (f x) returns False for all nodes of the path except for v and w
      -- If the Bool is False, the path may be of length 0, and if it is not
      --    then f v must be False.
      -- If the Bool is True, the path must be of length at least 1.

   getDescendants,
      -- :: Graph graph => Bool
      -- -> graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- -> (nodeLabel -> IO Bool) -> node -> IO [node]
      --  Same specification as getAncestors, with arc directions reversed.

   getAncestorsGeneric,
      -- :: Ord node => Bool -> (node -> IO [node]) -> (node -> IO Bool)
      -- -> node
      -- -> IO [node]
      -- general function for doing the above.

   isAncestorPure, -- :: Ord node => (node -> [node]) -> node -> node -> Bool
      -- Returns True if first node is ancestor or equal to the second.
   isAncestor, -- :: (Monad m,Ord node) => (node -> m [node]) -> node -> node
      -- -> m Bool
   getAncestorsPure,
      -- :: Ord node => (node -> [node]) -> node -> [node]
      -- This is a pure cut-down function for extracting a node's ancestors.
   ) where

import Control.Monad
import Data.Maybe

import qualified Data.Set as Set
import Data.Set (Set)

import Graphs.Graph


-- ------------------------------------------------------------------------
-- The functions
-- ------------------------------------------------------------------------

getAncestors
   :: Graph graph
   => Bool -> graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
getAncestors nonTrivial graph f1 node0 =
   let
      getParents :: Node -> IO [Node]
      getParents node =
         do
            arcs <- getArcsIn graph node
            mapM (getSource graph) arcs

      f :: Node -> IO Bool
      f node =
         do
            label <- getNodeLabel graph node
            f1 label
   in
      getAncestorsGeneric nonTrivial getParents f node0



getDescendants
   :: Graph graph
   => Bool -> graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
getDescendants nonTrivial graph f1 node0 =
   let
      getParents :: Node -> IO [Node]
      getParents node =
         do
            arcs <- getArcsOut graph node
            mapM (getTarget graph) arcs

      f :: Node -> IO Bool
      f node =
         do
            label <- getNodeLabel graph node
            f1 label
   in
      getAncestorsGeneric nonTrivial getParents f node0

-- ------------------------------------------------------------------------
-- getAncestorsGeneric
-- ------------------------------------------------------------------------

getAncestorsGeneric
   :: Ord node
   => Bool -> (node -> IO [node]) -> (node -> IO Bool) -> node
   -> IO [node]
getAncestorsGeneric nonTrivial getParents f node =
   do
      (visited,ancestors) <-
         (if nonTrivial then getAncestorsGenericInnerStrict
            else getAncestorsGenericInner)
         getParents f (Set.empty,[]) node
      return ancestors

getAncestorsGenericInner
   :: Ord node => (node -> IO [node]) -> (node -> IO Bool)
   -> (Set node,[node]) -> node -> IO (Set node,[node])
getAncestorsGenericInner getParents f (state @ (visitedSet0,ancestors0)) node =
   if Set.member node visitedSet0
      then
         return state
      else
         do
            let
               visitedSet1 = Set.insert node visitedSet0
            isAncestor <- f node
            if isAncestor
               then
                  return (visitedSet1,(node : ancestors0))
               else
                  getAncestorsGenericInnerStrict getParents f
                     (visitedSet1,ancestors0) node

getAncestorsGenericInnerStrict
   :: Ord node => (node -> IO [node]) -> (node -> IO Bool)
   -> (Set node,[node]) -> node -> IO (Set node,[node])
getAncestorsGenericInnerStrict getParents f (state @ (visitedSet0,ancestors0))
      node =
   do
      parents <- getParents node
      foldM
         (getAncestorsGenericInner getParents f)
         (visitedSet0,ancestors0)
         parents

-- | Returns True if first node is ancestor or equal to the second.
isAncestor :: (Monad m,Ord node) => (node -> m [node]) -> node -> node
   -> m Bool
isAncestor (getParents :: node -> m [node]) (node1 :: node) (node2 :: node) =
   let
      isAncestorInner :: Set node -> node -> m (Maybe (Set node))
      isAncestorInner visitedSet0 node =
         if Set.member node visitedSet0
            then
               return (
                  if node == node1
                     then
                        Nothing
                     else
                        Just visitedSet0
                  )
            else
               let
                  visitedSet1 :: Set node
                  visitedSet1 = Set.insert node visitedSet0
               in
                  do
                     parents <- getParents node
                     scanParents visitedSet1 parents

      scanParents :: Set node -> [node] -> m (Maybe (Set node))
      scanParents visitedSet0 [] = return (Just visitedSet0)
      scanParents visitedSet0 (node:nodes) =
         do
            search1Result <- isAncestorInner visitedSet0 node
            case search1Result of
               Nothing -> return Nothing
               Just visitedSet1 -> scanParents visitedSet1 nodes
   in
      do
         searchResultOpt <- isAncestorInner (Set.singleton node1) node2
         return (not (isJust searchResultOpt))
{-# SPECIALIZE isAncestor
   :: (Integer -> IO [Integer]) -> Integer -> Integer -> IO Bool #-}
-- this will be used for VersionState.versionIsAncestor

-- | Returns True if first node is ancestor or equal to the second.
isAncestorPure :: Ord node => (node -> [node]) -> node -> node -> Bool
isAncestorPure getParents (node1 :: node) (node2 :: node) =
   let
      isAncestorPureInner :: Set node -> node -> Maybe (Set node)
      isAncestorPureInner visitedSet0 node =
         if Set.member node visitedSet0
            then
               if node == node1
                  then
                     Nothing
                  else
                     Just visitedSet0
            else
               let
                  visitedSet1 :: Set node
                  visitedSet1 = Set.insert node visitedSet0
               in
                  scanParents visitedSet1 (getParents node)

      scanParents visitedSet0 [] = Just visitedSet0
      scanParents visitedSet0 (node:nodes) =
         case isAncestorPureInner visitedSet0 node of
            Nothing -> Nothing
            Just visitedSet1 -> scanParents visitedSet1 nodes
   in
      not (isJust (isAncestorPureInner (Set.singleton node1) node2))

getAncestorsPure :: Ord node => (node -> [node]) -> node -> [node]
getAncestorsPure getParents (node0 :: node) =
   let
      getAncestorsPureInner :: Set node -> node -> Set node
      getAncestorsPureInner visitedSet0 node =
         if Set.member node visitedSet0
            then
               visitedSet0
            else
               let
                  visitedSet1 = Set.insert node visitedSet0
                  parents = getParents node
               in
                  foldl getAncestorsPureInner visitedSet1 parents
   in
      Set.toList (getAncestorsPureInner Set.empty node0)

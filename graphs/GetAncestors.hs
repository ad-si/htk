module GetAncestors(
   getAncestors,
      -- :: Graph graph => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
      -- 
      -- Given an acyclic graph, a function (f :: nodeLabel -> IO Bool),
      -- and a node v, we return the set of nodes W such that for w in W,
      -- (1) (f w) returns True
      -- (2) there is a path, possibly of length 0, from w to v, such that
      --     (f x) returns False for all nodes of the path except for w

   getDescendants,
      -- :: Graph graph => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- -> (nodeLabel -> IO Bool) -> node -> IO [node]
      --  Same specification as getAncestors, with arc directions reversed.

   getAncestorsGeneric,
      -- :: Ord node => (node -> IO [node]) -> (node -> IO Bool) -> node 
      -- -> IO [node]
      -- general function for doing the above.
      
   ) where

import Monad

import Data.Set

import Graph


-- ------------------------------------------------------------------------
-- The functions
-- ------------------------------------------------------------------------

getAncestors 
   :: Graph graph => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
getAncestors graph f1 node0 =
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
      getAncestorsGeneric getParents f node0
    
   

getDescendants 
   :: Graph graph => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> (nodeLabel -> IO Bool) -> Node -> IO [Node]
getDescendants graph f1 node0 =
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
      getAncestorsGeneric getParents f node0

-- ------------------------------------------------------------------------
-- getAncestorsGeneric
-- ------------------------------------------------------------------------

getAncestorsGeneric 
   :: Ord node => (node -> IO [node]) -> (node -> IO Bool) -> node -> IO [node]
getAncestorsGeneric getParents f node = 
   do
      (visited,ancestors) <- 
         getAncestorsGenericInner getParents f (emptySet,[]) node
      return ancestors

getAncestorsGenericInner
   :: Ord node => (node -> IO [node]) -> (node -> IO Bool)  
   -> (Set node,[node]) -> node -> IO (Set node,[node])
getAncestorsGenericInner getParents f (state @ (visitedSet0,ancestors0)) node =
   if elementOf node visitedSet0
      then
         return state
      else
         do
            let
               visitedSet1 = addToSet visitedSet0 node
            isAncestor <- f node
            if isAncestor
               then
                  return (visitedSet1,(node : ancestors0))
               else
                  do
                     parents <- getParents node
                     foldM
                        (getAncestorsGenericInner getParents f)
                        (visitedSet1,ancestors0)                   
                        parents
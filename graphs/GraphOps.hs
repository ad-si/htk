-- | This module contains various functions for operating on graphs 
module GraphOps(
   isAncestor, -- :: graph ... -> Node -> Node -> IO Bool
      -- returns True if the first Node is an ancestor, or identical, to
      -- the second one.

   isAncestorBy, -- :: Ord key => (key -> IO [key]) -> key -> key -> IO Bool
      -- generic version
   ) where

import DeprecatedSet

import Queue

import Graph

-- ---------------------------------------------------------------------------
-- The functions
-- ---------------------------------------------------------------------------

isAncestor :: Graph graph 
   => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel 
   -> Node -> Node -> IO Bool
isAncestor graph node1 node2 =
   let
      getChildren :: Node -> IO [Node]
      getChildren node =
         do
            arcs <- getArcsOut graph node
            mapM (\ arc -> getTarget graph arc) arcs
   in
      isAncestorBy getChildren node1 node2
   

isAncestorBy :: Ord key => (key -> IO [key]) -> key -> key -> IO Bool
isAncestorBy getChildren (node1 :: node) node2 =
   do
      let
         -- The first argument is the visited set; the second the nodes
         -- to be done.  We use a queue so that the search is breadth-first
         -- not depth-first.
         search :: Set node -> Queue node -> IO Bool
         search visited toDo0 = case removeQ toDo0 of
            Nothing -> return False
            Just (node,toDo1) ->
               if elementOf node visited 
                  then
                     search visited toDo1
                  else
                     if node == node2
                        then
                           return True
                        else
                           do
                              children <- getChildren node
                              let
                                 toDo2 = foldl insertQ toDo1 children
                                 visited1 = addToSet visited node
                              search visited1 toDo2

      search emptySet (singletonQ node1)


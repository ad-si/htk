-- | The removeAncestors function in this module (actually an IO action) takes
-- a graph G and a list of nodes N and computes N' = { n in N |
--    there does not exist an m in N and a non-trivial path n -> m }.
-- This is required for graph merging. 
module RemoveAncestors(
   removeAncestors,
   removeAncestorsBy,
   removeAncestorsByPure,
   ) where

import Monad


import Control.Monad.Identity
import Data.FiniteMap

import Graph


-- | Takes a graph G and a list of nodes N and computes N' = { n in N |
-- there does not exist an m in N and a non-trivial path n -> m }.
removeAncestors :: Graph graph => 
   graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> [Node]
   -> IO [Node]
removeAncestors graph nodes =
   do
      let
         getChildren node =
            do
               arcsOut <- getArcsOut graph node
               mapM
                  (\ arc -> getTarget graph arc)
                  arcsOut

      removeAncestorsBy getChildren nodes


-- | General removeAncestors function, which takes as argument the action
-- computing a Node\'s successors.
removeAncestorsBy :: (Ord node,Monad m) 
   => (node -> m [node]) -> [node] -> m [node]
removeAncestorsBy (getChildren :: node -> m [node]) (nodes :: [node]) =
   do
      -- We maintain a state of type (FiniteMap node NodeState) to express
      -- what is currently known about each node.  We also maintain
      -- a set containing the target nodes.

      -- compute initial map
      let
         state0 = listToFM (map (\ node -> (node,Yes)) nodes)

         uniqueNodes = map fst (fmToList state0)

         -- Return True if there is a, possibly trivial, path from this node
         -- to one of the target set, also transforming the state.
         -- EXCEPTION - we don't search down nodes which have Cycle set,
         -- and in that case return False.
         nodeIsAncestor :: node -> FiniteMap node NodeState
            -> m (Bool,FiniteMap node NodeState)
         nodeIsAncestor node state0 =
            case lookupFM state0 node of
               Just Yes -> return (True,state0)
               Just No -> return (False,state0)
               Just Cycle -> return (False,state0)
               Nothing ->
                  do
                     let
                        state1 = addToFM state0 node Cycle

                     children <- getChildren node
                     (isAncestor,state2) <- anyNodeIsAncestor children state1
                     let
                        state3 = addToFM state2 node
                           (if isAncestor then Yes else No)
                     return (isAncestor,state3)  

         -- Returns True if there is a, possibly trivial, path from any
         -- of the given nodes to one of the target nodes.
         anyNodeIsAncestor :: [node] -> FiniteMap node NodeState
            -> m (Bool,FiniteMap node NodeState)
         anyNodeIsAncestor [] state0 = return (False,state0)
         anyNodeIsAncestor (node : nodes) state0 =
            do
               (thisIsAncestor,state1) <-  nodeIsAncestor node state0
               if thisIsAncestor 
                  then
                     return (True,state1)
                  else
                     anyNodeIsAncestor nodes state1

         -- Returns True if there is a non-trivial path from the given node
         -- to one of the target nodes.
         nodeIsNonTrivialAncestor :: node -> FiniteMap node NodeState
            -> m (Bool,FiniteMap node NodeState)
         nodeIsNonTrivialAncestor node state0 =
            do
               children <- getChildren node
               anyNodeIsAncestor children state0

      (list :: [node],finalState :: FiniteMap node NodeState) <- foldM
         (\ (listSoFar,state0) node ->
            do
               (isAncestor,state1) <- nodeIsNonTrivialAncestor node state0
               return (if isAncestor then (listSoFar,state1) 
                  else (node:listSoFar,state1))
            )
         ([],state0)
         uniqueNodes
  
      return list

-- | Pure version of 'removeAncestorsBy'.
removeAncestorsByPure :: Ord node => (node -> [node]) -> [node] -> [node]
removeAncestorsByPure (toParents0 :: node -> [node]) nodes =
   let
      toParents1 :: node -> Identity [node]
      toParents1 = Identity . toParents0
   in
      runIdentity (removeAncestorsBy toParents1 nodes)

-- | This describes the information kept about a node during the course of
-- removeAncestorsBy
data NodeState = 
      Yes -- ^ there is a, possibly trivial, path from here to an element
          -- of the target set.
   |  No  -- ^ the opposite of Yes.
   |  Cycle -- ^ we are already searching from this element.

{- SPECIALIZE removeAncestorsBy 
   ::  (Node -> IO [Node]) -> [Node] -> IO [Node] -}

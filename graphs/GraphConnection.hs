{- GraphConnection contains various operations on graph connections -}
module GraphConnection(
   SubGraph(..), 
      -- defines a subgraph as a subset of nodes and node types.
      -- The user is responsible for making sure that if a node is in
      -- the subgraph, so is its type!
   attachSuperGraph, 
      -- :: SubGraph 
      --    -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      --    -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- turn a graph connection into one, which when passing information back
      -- to the parent, ignores updates which don't lie in the subgraph.
   attachSubGraph,
      -- :: SubGraph
      --    -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      --    -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- Ditto, but only accepts updates (and parts of the original graph)
      -- which lie in the original graph.
   ) where

import Monad(filterM)

import Set
import Concurrent

import Computation (done)

import Graph

-----------------------------------------------------------------------------
-- Connection State
-----------------------------------------------------------------------------

-- We keep track of the arcs currently not in the subgraph; this
-- allows us to filter out attempts to delete from the subgraph.
-- (Attempts to delete non-existent arcs is normally harmless but,
-- apart from wasting time, could get passed onto other clients of the
-- server, which might have themselves constructed identical arcs
-- in their subgraphs.)
newtype ConnectionState = ConnectionState (MVar (Set Arc))
-- This contains the arcs NOT in the subgraph, because for our planned
-- application 

newConnectionState :: IO ConnectionState
newConnectionState = 
   do
      mVar <- newMVar emptySet
      return (ConnectionState mVar) 

arcIsInSubGraph :: ConnectionState -> Arc -> IO Bool
arcIsInSubGraph (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      let
         result = not (elementOf arc set)
      putMVar mVar set 
      return result

arcAdd :: ConnectionState -> Arc -> IO ()
arcAdd (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      putMVar mVar (union set (unitSet arc))

arcDelete :: ConnectionState -> Arc -> IO ()
arcDelete (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      putMVar mVar (minusSet set (unitSet arc))

-----------------------------------------------------------------------------
-- SubGraph
-----------------------------------------------------------------------------

data SubGraph = SubGraph {
   nodeIn :: Node -> Bool,
   nodeTypeIn :: NodeType -> Bool
   }

updateIsInSubGraph :: SubGraph -> ConnectionState 
   -> Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel -> IO Bool
updateIsInSubGraph (SubGraph{nodeIn = nodeIn,nodeTypeIn = nodeTypeIn}) 
      connectionState update =
   case update of
      NewNodeType nodeType _ -> return (nodeTypeIn nodeType)
      SetNodeTypeLabel nodeType _ -> return (nodeTypeIn nodeType)
      NewNode node _ _ -> return (nodeIn node)
      DeleteNode node -> return (nodeIn node)
      SetNodeLabel node _ -> return (nodeIn node)
      NewArc arc _ _ node1 node2 -> 
         do
            let
               inSubGraph = nodeIn node1 && nodeIn node2
            if inSubGraph 
               then
                  return True
               else
                  do
                     arcAdd connectionState arc
                     return False
      DeleteArc arc ->
         do
            inSubGraph <- arcIsInSubGraph connectionState arc
            if inSubGraph
               then
                  return True
               else
                  do
                     arcDelete connectionState arc
                     return False
      SetArcLabel arc _ -> arcIsInSubGraph connectionState arc      
      _ -> return True

-----------------------------------------------------------------------------
-- GraphConnection operations
-----------------------------------------------------------------------------

attachSuperGraph :: SubGraph 
   -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
attachSuperGraph subGraph graphConnection parentChanges =
   do
      graphConnectionData <- graphConnection parentChanges
      -- all changes to the parent get passed on

      connectionState <- newConnectionState
      let 
         oldGraphUpdate = graphUpdate graphConnectionData

         newGraphUpdate update =
            -- updates to the child only get passed on if in the subgraph.
            do
               isInSubGraph 
                  <- updateIsInSubGraph subGraph connectionState update
               if isInSubGraph
                  then
                     oldGraphUpdate update
                  else
                     done 
      return (graphConnectionData {graphUpdate = newGraphUpdate})

attachSubGraph :: SubGraph
   -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> GraphConnection nodeLabel nodeTypeLabel arcLabel arcTypeLabel
attachSubGraph subGraph graphConnection parentChanges =
   do
      connectionState <- newConnectionState
      let
         newParentChanges update =
         -- Changes from the parent only get passed on if in the subgraph.
            do
               isInSubGraph 
                  <- updateIsInSubGraph subGraph connectionState update  
               if isInSubGraph
                  then
                     parentChanges update
                  else
                     done 
      graphConnectionData <- graphConnection newParentChanges
      -- We have to filter the graph state.
      let
         oldGraphState = graphState graphConnectionData
         oldUpdates = updates oldGraphState
      newUpdates <- filterM (updateIsInSubGraph subGraph connectionState)
         oldUpdates
      let
         newGraphState = CannedGraph {updates = newUpdates}
      return (graphConnectionData {graphState = newGraphState})

{-# LANGUAGE ScopedTypeVariables #-}

-- | GraphConnection contains various operations on graph connections
module Graphs.GraphConnection(
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

   mapGraphConnection,
   ) where

import Control.Monad(filterM)

import qualified Data.Set as Set
import Control.Concurrent

import Util.Computation (done)

import Graphs.Graph

-----------------------------------------------------------------------------
-- Connection State
-----------------------------------------------------------------------------

-- We keep track of the arcs currently not in the subgraph; this
-- allows us to filter out attempts to delete from the subgraph.
-- (Attempts to delete non-existent arcs is normally harmless but,
-- apart from wasting time, could get passed onto other clients of the
-- server, which might have themselves constructed identical arcs
-- in their subgraphs.)
newtype ConnectionState = ConnectionState (MVar (Set.Set Arc))
-- This contains the arcs NOT in the subgraph, because for our planned
-- application

newConnectionState :: IO ConnectionState
newConnectionState =
   do
      mVar <- newMVar Set.empty
      return (ConnectionState mVar)

arcIsInSubGraph :: ConnectionState -> Arc -> IO Bool
arcIsInSubGraph (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      let
         result = not (Set.member arc set)
      putMVar mVar set
      return result

arcAdd :: ConnectionState -> Arc -> IO ()
arcAdd (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      putMVar mVar (Set.union set (Set.singleton arc))

arcDelete :: ConnectionState -> Arc -> IO ()
arcDelete (ConnectionState mVar) arc =
   do
      set <- takeMVar mVar
      putMVar mVar (Set.difference set (Set.singleton arc))

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
      SetNodeType node _ -> return (nodeIn node)
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
      SetArcType arc _ -> arcIsInSubGraph connectionState arc
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

{-# DEPRECATED attachSuperGraph,attachSubGraph
   "Functions need to be updated to cope with MultiUpdate" #-}

--  | Throw away the old types in a graph, and recompute them from the
-- node and arc labels.
mapGraphConnection ::
   (nodeLabel1 -> (nodeLabel2,NodeType))
      -- ^ function to compute node label in new graph and type
   -> (arcLabel1 -> (arcLabel2,ArcType))
      -- ^ function to compute arc label in new graph and type
   -> [Update nodeLabel2 nodeTypeLabel2 arcLabel2 arcTypeLabel2]
      -- ^ updates prepended to initialse types.
      -- (The type declarations in the input graph are discarded)
   -> GraphConnection nodeLabel1 () arcLabel1 ()
   -> GraphConnection nodeLabel2 nodeTypeLabel2 arcLabel2 arcTypeLabel2
   -- NB.  Changes to the child do not get passed back.
mapGraphConnection
      (mapNode :: nodeLabel1 -> (nodeLabel2,NodeType))
      (mapArc :: arcLabel1 -> (arcLabel2,ArcType))
      (initialUpdates
         :: [Update nodeLabel2  nodeTypeLabel2 arcLabel2 arcTypeLabel2])
      graphConnection1 updateFn2 =
   let
      mapUpdate :: Update nodeLabel1 () arcLabel1 ()
         -> Update nodeLabel2 nodeTypeLabel2 arcLabel2 arcTypeLabel2
      mapUpdate update = case update of
         NewNodeType _ _ -> nop
         SetNodeTypeLabel _ _ -> nop
         NewNode node _ nodeTypeLabel1 ->
            let
               (nodeTypeLabel2,nodeType2) = mapNode nodeTypeLabel1
            in
               NewNode node nodeType2 nodeTypeLabel2
         DeleteNode node -> DeleteNode node
         SetNodeLabel node nodeLabel1 ->
            let
               (nodeLabel2,nodeType2) = mapNode nodeLabel1
            in
               MultiUpdate [
                  SetNodeLabel node nodeLabel2,
                  SetNodeType node nodeType2
                  ]
         SetNodeType _ _ -> nop
         NewArcType _ _ -> nop
         SetArcTypeLabel _ _ -> nop
         NewArc arc _ arcLabel1 nodeFrom nodeTo ->
            let
               (arcLabel2,arcType2) = mapArc arcLabel1
            in
               NewArc arc arcType2 arcLabel2 nodeFrom nodeTo
         DeleteArc arc -> DeleteArc arc
         SetArcLabel arc arcLabel1 ->
            let
               (arcLabel2,arcType2) = mapArc arcLabel1
            in
               MultiUpdate [
                  SetArcLabel arc arcLabel2,
                  SetArcType arc arcType2
                  ]
         SetArcType _ _ -> nop
         MultiUpdate updates -> MultiUpdate (fmap mapUpdate updates)

      updateFn1 update1 = updateFn2 (mapUpdate update1)

      nop = MultiUpdate []
   in
      do
         graphConnectionData1 <- graphConnection1 updateFn1
         let
            cannedGraph1 = graphState graphConnectionData1
            updates1 = updates cannedGraph1
            updates2 = initialUpdates ++ fmap mapUpdate updates1
            cannedGraph2 = CannedGraph {updates = updates2}
            graphUpdate2 _ = done

            graphConnectionData2 = graphConnectionData1 {
               graphState = cannedGraph2,
               graphUpdate = graphUpdate2
               }
         return graphConnectionData2

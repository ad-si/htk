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

import Computation (done)

import Graph

-----------------------------------------------------------------------------
-- SubGraph
-----------------------------------------------------------------------------

data SubGraph = SubGraph {
   nodeIn :: Node -> Bool,
   nodeTypeIn :: NodeType -> Bool
   }

updateIsInSubGraph :: SubGraph 
   -> Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel -> Bool
updateIsInSubGraph (SubGraph{nodeIn = nodeIn,nodeTypeIn = nodeTypeIn}) update =
-- NB: we can't prevent arc operations with nodes that aren't in the subgraph
-- getting passed on.  But, at any rate if the parent is a SimpleGraph, the
-- operations will not get passed on further.
-- (With the applications I have in mind, SetArcLabel and DeleteArc won't
-- be very common anyway, so this probably won't be too much of a problem.)
   case update of
      NewNodeType nodeType _ -> nodeTypeIn nodeType
      SetNodeTypeLabel nodeType _ -> nodeTypeIn nodeType
      NewNode node _ _ -> nodeIn node
      DeleteNode node -> nodeIn node
      SetNodeLabel node _ -> nodeIn node
      NewArc _ _ _ node1 node2 -> nodeIn node1 && nodeIn node2
      _ -> True

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
      let 
         oldGraphUpdate = graphUpdate graphConnectionData

         newGraphUpdate update =
            -- updates to the child only get passed on if in the subgraph.
            if updateIsInSubGraph subGraph update
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
      let
         newParentChanges update =
         -- Changes from the parent only get passed on if in the subgraph.
            if updateIsInSubGraph subGraph update
               then
                  parentChanges update
               else
                  done 
      graphConnectionData <- graphConnection newParentChanges
      -- We have to filter the graph state.
      let
         oldGraphState = graphState graphConnectionData
         newGraphState =
            CannedGraph {
               updates = filter (updateIsInSubGraph subGraph)
                  (updates oldGraphState)
               }
      return (graphConnectionData {graphState = newGraphState})

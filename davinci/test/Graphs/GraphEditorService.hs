-- | This provides a ServiceClass instance allowing our
-- GraphEditor to be run across a network.
module Graphs.GraphEditorService(
   graphEditorService, -- :: pass to connectBroadcastOther to call server
   graphEditorServiceWrapped, -- pass to server
   FrozenGraph(..), -- What is sent (Showed) on client connect.
   ) where

import Util.Computation(done)

import Util.Thread(secs)

import Server.ServiceClass

import Graphs.NewNames
import Graphs.Graph
import Graphs.SimpleGraph
import Graphs.GraphEditor
import Util.BinaryAll

data FrozenGraph = FrozenGraph {
   graphState' :: DisplayableCannedGraph,
   nameSourceBranch' :: NameSourceBranch
   } deriving (Read,Show) -- sent on branching

data FrozenGraph2 = FrozenGraph2 {
   graphState2 :: DisplayableCannedGraph,
   nameSource2 :: FrozenNameSource
   } deriving (Read,Show) -- used for backups.

graphEditorServiceWrapped :: Service
graphEditorServiceWrapped = Service graphEditorService

graphEditorService :: (ReadShow DisplayableUpdate,ReadShow DisplayableUpdate,
      Displayable SimpleGraph)
graphEditorService = serviceArg

instance ServiceClass
   (ReadShow DisplayableUpdate) (ReadShow DisplayableUpdate)
   (Displayable SimpleGraph) where

   serviceId _ = "GraphEditor"

   serviceMode _ = BroadcastOther

   handleRequest _ _ (ReadShow newUpdate,simpleGraph) =
      do
         update simpleGraph newUpdate
         return (ReadShow newUpdate,simpleGraph)

   getBackupDelay _ = return (BackupAfter (secs 2.0))

   sendOnConnect _ _ simpleGraph =
      do
         let
             graphConnection = shareGraph simpleGraph
             newUpdate _ = error "Update in mid-sendOnConnect!"
         -- shouldn't happen because we deregister updates
         -- and handleRequest won't be called again until sendOnConnect
         -- is finished.
         GraphConnectionData {
            graphState = graphState',
            deRegister = deRegister,
            nameSourceBranch = nameSourceBranch'
            } <- graphConnection newUpdate
         deRegister
         return (show (FrozenGraph {
            graphState' = graphState',
            nameSourceBranch' = nameSourceBranch'
            }))

   initialStateFromString _ Nothing = newEmptyGraph
   initialStateFromString _ (Just frozenGraphString) =
      do
         let
            FrozenGraph2 {
               graphState2 = graphState,
               nameSource2 = nameSource2
               } = read frozenGraphString

            graphConnection childUpdates =
               return (GraphConnectionData {
                  graphState = graphState,
                  deRegister = done,
                  graphUpdate = (\ _ -> done),
                  nameSourceBranch = initialBranch
                  })
         graph <- newGraph graphConnection
         -- Hack the name source
         let
            nameSource = getNameSource graph

         defrostNameSource nameSource nameSource2

         return graph

   backupToString _ graph =
      do
         let
            nameSource = getNameSource graph
         frozenNameSource <- freezeNameSource nameSource
         (GraphConnectionData {
            graphState = graphState,
            deRegister = deRegister,
            graphUpdate = graphUpdate
            }) <- shareGraph graph (\ update -> done)

         -- Avoid wasting a branch number
         defrostNameSource nameSource frozenNameSource
         deRegister
         return (show (FrozenGraph2 {
            graphState2 = graphState,
            nameSource2 = frozenNameSource
            }))



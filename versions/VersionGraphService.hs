{- This provides a ServiceClass instance for the Version graph.
   It is fairly similar to GraphEditorService, except for the types.
   Specifically
      nodes are labelled with String.
      arcs are labelled with String.
      node & arc types are labelled with ().
      
      The nodes correspond to checked-in versions, and the 
   -}
module VersionGraphService(
   versionGraphService, -- :: pass to connectBroadcastOther to call server
   versionGraphServiceWrapped, -- pass to server
   FrozenGraph(..), -- What is sent (Showed) on client connect.
   ) where

import Computation(done)

import Thread(secs)

import ServiceClass

import NewNames
import Graph
import SimpleGraph
import FileSys(Version)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type VersionTypes dataSort = dataSort String () String () 
   -- change to alter node/nodetype/arc/arctypes.

type VersionGraph = VersionTypes SimpleGraph

type VersionUpdate = VersionTypes Update

type CannedVersionGraph = VersionTypes CannedGraph

data FrozenGraph = FrozenGraph {
   graphState' :: CannedVersionGraph,
   nameSourceBranch' :: NameSourceBranch
   } deriving (Read,Show) -- sent on branching

data FrozenGraph2 = FrozenGraph2 {
   graphState2 :: CannedVersionGraph,
   nameSource2 :: FrozenNameSource
   } deriving (Read,Show) -- used for backups.

versionGraphServiceWrapped = Service versionGraphService

versionGraphService = serviceArg :: (VersionUpdate,VersionUpdate,VersionGraph)


------------------------------------------------------------------------------
-- The instance
------------------------------------------------------------------------------


instance ServiceClass VersionUpdate VersionUpdate VersionGraph where

   serviceId _ = "VersionGraph"

   serviceMode _ = BroadcastOther

   handleRequest _ (newUpdate,simpleGraph) =
      do
         update simpleGraph newUpdate
         return (newUpdate,simpleGraph)

   getBackupDelay _ = return (BackupAfter (secs 2.0))

   sendOnConnect _ simpleGraph =
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
            graphUpdate = graphUpdate,
            }) <- shareGraph graph (\ update -> done)

         -- Avoid wasting a branch number
         defrostNameSource nameSource frozenNameSource

         deRegister

         return (show (FrozenGraph2 {
            graphState2 = graphState,
            nameSource2 = frozenNameSource
            }))


{- This provides a ServiceClass instance allowing our
   GraphEditor to be run across a network.  

   There is no backing-up service, but there should be.  As it is
   we start a new graph each time.
   -}
module GraphEditorService(
   graphEditorService, -- :: pass to connectBroadcastOther to call server
   FrozenGraph, -- What is sent (Showed) on client connect.
   ) where

import ServiceClass

import NewNames
import Graph
import SimpleGraph
import GraphEditor

data FrozenGraph = FrozenGraph {
   graphState' :: DisplayableCannedGraph,
   nameSourceBranch' :: NameSourceBranch
   } deriving (Read,Show)


graphEditorService = serviceArg :: (DisplayableUpdate,DisplayableUpdate,
   Displayable SimpleGraph)


instance ServiceClass DisplayableUpdate DisplayableUpdate 
   (Displayable SimpleGraph) where

   serviceId _ = "GraphEditor"

   serviceMode _ = BroadcastOther

   initialState _ = newEmptyGraph

   handleRequest _ (newUpdate,simpleGraph) =
      do
         update simpleGraph newUpdate
         return (newUpdate,simpleGraph)

   getBackupDelay _ = return BackupNever

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
   

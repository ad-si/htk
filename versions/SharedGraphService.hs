{- Here we implement a Service that shares data between graphs.
   This is like the standard Echo server except that we
   keep internally a SharedGraph.  When we receive a new
   connection, we send it a canned version of this SharedGraph.
   -}
module SharedGraph(
   sharedGraphService,
   sharedGraphServiceWrapped
   ) where

import ServiceClass

import Selection(inaction)

import SharedGraph

sharedGraphService = serviceArg :: (Update,Update,SharedGraph)
sharedGraphServiceWrapped = Service echoService 

instance ServiceClass Update Update SharedGraph where
   serviceId _ = "SharedGraph"
   serviceMode _ = Broadcast
   initialState _ = makeSharedGraph emptyCannedGraph inaction
   handleRequest _ (update,sharedGraph) =
      do
         updateSharedGraph sharedGraph update
         return (update,sharedGraph)
   sendOnConnect _ sharedGraph =
      do
         cannedGraph <- canGraph sharedGraph
         return (show cannedGraph)
         
   getBackupDelay _ = return BackupNever


{- Here we implement a Service that shares data between graphs.
   This is like the standard Echo server except that we
   keep internally a SharedGraph.  When we receive a new
   connection, we send it a canned version of this SharedGraph.
 
   The Service is parameterised on the nodeLabel type, allowing
   us to have more than one graph for different node labels.  But
   the nodeLabel must be an instance of Typeable.
   -}
module SharedGraphService(
   sharedGraphService,
   sharedGraphServiceWrapped
   ) where

import Dynamics

import Selective(inaction)

import ServiceClass

import SharedGraph

sharedGraphService :: (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) =>
   (Update nodeLabel,Update nodeLabel,SharedGraph nodeLabel)
sharedGraphService = serviceArg 

sharedGraphServiceWrapped 
   :: (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) => 
   nodeLabel -> Service
sharedGraphServiceWrapped (_ :: nodeLabel) = 
   Service (sharedGraphService :: 
      (Update nodeLabel,Update nodeLabel,SharedGraph nodeLabel))

instance (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) =>
      ServiceClass (Update nodeLabel) (Update nodeLabel) 
      (SharedGraph nodeLabel) where
   serviceId _ = "SharedGraph"++
      let
         (nodeLabelTop :: nodeLabel) = nodeLabelTop
      in
         return(show(typeOf nodeLabelTop))

   serviceMode _ = Broadcast
   initialState _ = makeSharedGraph (emptyCannedGraph,inaction)
   handleRequest _ (update,sharedGraph) =
      do
         updateSharedGraph sharedGraph update
         return (update,sharedGraph)
   sendOnConnect _ sharedGraph =
      do
         (cannedGraph,event,stopEvent) <- shareGraph sharedGraph
         stopEvent
         return (show cannedGraph)
         
   getBackupDelay _ = return BackupNever


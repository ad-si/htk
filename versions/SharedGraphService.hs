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
   -- Both sharedGraphService and sharedGraphServiceWrapped take
   -- a type parameter with type the type of the node label,
   -- which isn't looked at.
   ) where

import Dynamics

import Selective(inaction)

import ServiceClass

import SharedGraph
import Graph

sharedGraphService :: (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) =>
   nodeLabel -> 
   (Update String nodeLabel,Update String nodeLabel,SharedGraph nodeLabel)
sharedGraphService (_ :: nodeLabel) = 
   serviceArg :: 
      (Update String nodeLabel,Update String nodeLabel,SharedGraph nodeLabel)


sharedGraphServiceWrapped 
   :: (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) => 
   nodeLabel -> Service
sharedGraphServiceWrapped nodeLabel = 
   Service (sharedGraphService nodeLabel)

instance (Typeable nodeLabel,Read nodeLabel,Show nodeLabel) =>
      ServiceClass (Update String nodeLabel) (Update String nodeLabel) 
      (SharedGraph nodeLabel) where
   serviceId _ = "SharedGraph"++
      let
         (nodeLabelTop :: nodeLabel) = nodeLabelTop
      in
         show(typeOf nodeLabelTop)

   serviceMode _ = Broadcast
   initialState _ = makeSharedGraph (emptyCannedGraph,inaction)
   handleRequest _ (update,sharedGraph) =
      do
         (updateWithNodes :: Update Node nodeLabel) <-
            mapMUpdate toNode update
         updateGraph sharedGraph updateWithNodes
         return (update,sharedGraph)
   sendOnConnect _ sharedGraph =
      do
         (cannedGraph,event,stopEvent) <- shareGraph sharedGraph
         stopEvent
         return (show cannedGraph)
         
   getBackupDelay _ = return BackupNever


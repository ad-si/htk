{- The SharedGraphClient is an implementation of the
   Graph structure for a client connected to a 
   SharedGraphService server.
   -}
module SharedGraphClient(
   SharedGraphClient, -- instance of Graph
   -- This does the connection
   connectSharedGraph 
   -- :: (Typeable nodeLabel,Read nodeLabel,Show nodeLabel,
   --       DescribesHost host,DescribesPort port) => host -> port ->
   --       nodeLabel -> IO (SharedGraphClient nodeLabel)
   -- The nodeLabel is a type parameter which isn't looked at.
   ) where

import Dynamics

import Thread
import Selective(inaction)
import SocketEV(DescribesHost,DescribesPort)

import CallServer

import Graph
import SharedGraph
import SharedGraphService

data SharedGraphClient nodeLabel  =
-- nodeLabel will in fact always be an instance of Typeable, Read, Show
-- because you can only construct a SharedGraphClient with connectSharedGraph.
-- But we cannot put that in the datatype or we couldn't make 
-- SharedGraphClient an instance of Graph.
   SharedGraphClient {
      sharedGraph :: SharedGraph nodeLabel, -- contains the current contents
      updateAction :: Update Node nodeLabel -> IO ()
      }

instance Graph SharedGraphClient where  
   getNodes graph = getNodes (sharedGraph graph)
   getPredecessors graph = getPredecessors (sharedGraph graph)
   getSuccessors graph = getPredecessors (sharedGraph graph)
   getLabel graph = getLabel (sharedGraph graph)
   shareGraph graph = shareGraph (sharedGraph graph)
   updateGraph graph update = (updateAction graph) update

------------------------------------------------------------------------
-- connectSharedGraph
------------------------------------------------------------------------

connectSharedGraph :: 
      (Typeable nodeLabel,Read nodeLabel,Show nodeLabel,
      DescribesHost host,DescribesPort port) =>
      host -> port -> nodeLabel -> IO (SharedGraphClient nodeLabel)
connectSharedGraph host port nodeLabel =
   do
      (updateAction,newUpdates,closeAction,cannedGraphString) <-
         connectBroadcast (sharedGraphService nodeLabel) host port
      let
         cannedGraph = read cannedGraphString
      sharedGraph <- makeSharedGraph (cannedGraph,inaction)
      let
         monitorUpdates =
            do
               update <- newUpdates
               updateNode <- mapMUpdate toNode update
               updateGraph sharedGraph updateNode
               monitorUpdates
         updateActionNode updateNode =
            do
               update <- mapMUpdate fromNode updateNode
               updateAction update

      forkIO monitorUpdates
      return (SharedGraphClient {
         sharedGraph = sharedGraph,
         updateAction = updateActionNode
         })
           
 
      
        
   






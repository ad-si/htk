{- The SharedGraph module contains code for sharing a labelled
   modifiable directed graph. -}
module SharedGraph(

   -- SharedGraphs (which change) and CannedGraphs (which don't).
   -- CannedGraphs are used for transferring SharedGraphs around.
   SharedGraph, 
      -- SharedGraph takes a parameter nodeLabel
   
   CannedGraph,
      -- contains complete contents of SharedGraph at some time
      -- also takes a parameter nodeLabel.  nodeLabel must be 
      -- an instance of Read/Show and so is CannedGraph
   emptyCannedGraph, -- :: CannedGraph nodeLabel 

   shareGraph, -- :: (Read nodeLabel,Show nodeLabel) => 
             --    SharedGraph nodeLabel -> 
             --       IO (CannedGraph nodeLabel,
             --          EV(Update Node nodeLabel),IO())
             --    Returns (a) a canned graph; (b) an event conveying updates
             --    to that graph; (c) an action which you should perform
             --    when you want the event to stop being generated.

   makeSharedGraph, -- :: (Read nodeLabel,Show nodeLabel) =>
             --    (CannedGraph nodeLabel,EV(Update Node nodeLabel)) ->
             --       IO (SharedGraph nodeLabel)
             --    makeSharedGraph creates a shared graph given a 
             --    canned graph plus a source of updates.

   ) where

import FiniteMap
import Concurrency
import Concurrent(readMVar)

import ExtendedPrelude
import SmallSet
import QuickReadShow
import EasyBroker

import SIM

import Graph

------------------------------------------------------------------------
-- CannedGraph
------------------------------------------------------------------------

data (Read nodeLabel,Show nodeLabel) => CannedGraph nodeLabel =
   CannedGraph [CannedGraphNode nodeLabel] deriving (Read,Show)

data (Read nodeLabel,Show nodeLabel) => CannedGraphNode nodeLabel =
   CannedGraphNode {
      nodeString :: String,
      successorStrings :: [String],
      nodeLabel :: nodeLabel
      }

instance (Read nodeLabel,Show nodeLabel) => 
      QuickRead (CannedGraphNode nodeLabel) where
   quickRead = WrapRead (\ (nodeString,successorStrings,nodeLabel) ->
      CannedGraphNode {
         nodeString = nodeString,
         successorStrings = successorStrings,
         nodeLabel = nodeLabel
         }
      )

instance (Read nodeLabel,Show nodeLabel) => 
      QuickShow (CannedGraphNode nodeLabel) where
   quickShow = WrapShow (\ cgn ->
      (nodeString cgn,successorStrings cgn,nodeLabel cgn)
      )

emptyCannedGraph :: (Read nodeLabel,Show nodeLabel) => CannedGraph nodeLabel
emptyCannedGraph = CannedGraph []

------------------------------------------------------------------------
-- SharedGraph
------------------------------------------------------------------------

data SharedGraph nodeLabel = SharedGraph {
   nodeLookUp :: MVar (NodeMap nodeLabel),
   -- the MVar should at any time contain a graph with a consistent
   -- set of predecessors and successors.
   updateSource :: EasyBroker (Update Node nodeLabel)
   }

-- shareGraph and updateSharedGraph need
-- to synchronise carefully to make sure that for example the
-- event returned by shareGraph contains all updates.  So we do this
-- by locking on nodeLookUp.  
shareGraph :: (Read nodeLabel,Show nodeLabel) => 
   SharedGraph nodeLabel -> 
   IO (CannedGraph nodeLabel,EV(Update Node nodeLabel),IO())
shareGraph SharedGraph{
   nodeLookUp=nodeLookUp,
   updateSource=updateSource
   } =
   do
      channel <- newChannel
      eventSink <- newEventSink (send channel)
      -- lock
      nodeMap <- takeMVar nodeLookUp
      registerEventSink updateSource eventSink
      putMVar nodeLookUp nodeMap
      -- unlock
      cannedGraph <- canGraph nodeMap
      return (cannedGraph,receive channel,
         deregisterEventSink updateSource eventSink)
      
updateSharedGraph :: SharedGraph nodeLabel -> Update Node nodeLabel -> IO ()
updateSharedGraph 
      (SharedGraph{nodeLookUp=nodeLookUp,updateSource=updateSource}) 
      update =
   do
      -- lock
      nodeMap <- takeMVar nodeLookUp
      let
         nodeMap2 = applyEdit update nodeMap
      dispatchMessage updateSource update
      putMVar nodeLookUp nodeMap2
      -- unlock

makeSharedGraph :: (Read nodeLabel,Show nodeLabel) =>
      (CannedGraph nodeLabel,EV(Update Node nodeLabel)) -> 
      IO (SharedGraph nodeLabel)
makeSharedGraph (cannedGraph,updateEvent) =
   do
      nodeMap <- uncanGraph cannedGraph
      nodeLookUp <- newMVar nodeMap
      updateSource <- newEasyBroker
      let
         sharedGraph =
            SharedGraph{
               nodeLookUp=nodeLookUp,
               updateSource=updateSource
               }

         monitorChanges =
            do
               update <- sync updateEvent
               updateSharedGraph sharedGraph update
               monitorChanges

      forkIO monitorChanges

      return sharedGraph
 
------------------------------------------------------------------------
-- Constructing and deconstructing CannedGraph's.
------------------------------------------------------------------------

uncanGraph :: (Read nodeLabel,Show nodeLabel) => 
   CannedGraph nodeLabel -> IO (NodeMap nodeLabel)
uncanGraph (CannedGraph cannedGraphNodes) =
   do
      let
         cannedGraphNodeToUpdate(CannedGraphNode{
            nodeString = nodeString,
            successorStrings = successorStrings,
            nodeLabel = nodeLabel
            }) =
            do
               nodeAtom <- toNode nodeString
               successorAtoms <- mapM toNode successorStrings
               return(EditNode{
                  node = nodeAtom,
                  predecessorsOpt = Nothing,
                  successorsOpt = 
                     Just (addSmallSetList successorAtoms emptySmallSet),
                  nodeLabelOpt = Just nodeLabel
                  })
      updates <- mapM cannedGraphNodeToUpdate cannedGraphNodes
      let
         nodeMap =
            foldr
               applyEdit
               emptyNodeMap
               updates
      return nodeMap

canGraph :: (Read nodeLabel,Show nodeLabel) => 
   NodeMap nodeLabel -> IO (CannedGraph nodeLabel)
canGraph nodeMap =
   do
      let
         canNode (node,NodeData{successors=successors,label=label}) =
            do
               nodeString <- fromNode node
               successorStrings <- 
                  mapM
                     (\ node -> fromNode node)
                     (listSmallSet successors)   
               return(CannedGraphNode{
                  nodeString=nodeString,
                  successorStrings=successorStrings,
                  nodeLabel=label
                  })
         allNodeData = listNodeMap nodeMap    

      cannedNodes <-
         sequence(map canNode allNodeData)
  
      return(CannedGraph cannedNodes)

------------------------------------------------------------------------
-- NodeMap's (operations as maps)
-- We encapsulate the FiniteMap here, so it can possibly be
-- changed later.  So NodeMap should be abstract to the rest of the code.
------------------------------------------------------------------------

newtype NodeMap nodeLabel = NodeMap (FiniteMap Node (NodeData nodeLabel))

emptyNodeMap = NodeMap emptyFM
   
listNodeMap :: NodeMap nodeLabel -> [(Node,NodeData nodeLabel)]
listNodeMap (NodeMap nodeMap) = fmToList nodeMap

lookupDefault :: NodeMap nodeLabel -> Node -> NodeData nodeLabel
lookupDefault (NodeMap nodeMap) node = 
   lookupWithDefaultFM nodeMap defaultNodeData node

lookupError :: NodeMap nodeLabel -> Node -> NodeData nodeLabel
lookupError (NodeMap nodeMap) node =
   lookupWithDefaultFM nodeMap 
      (error "SharedGraph.lookupError - node not found")
      node

addNode :: NodeMap nodeLabel -> Node -> NodeData nodeLabel -> 
   NodeMap nodeLabel
addNode (NodeMap nodeMap) node nodeData =
   NodeMap (addToFM nodeMap node nodeData)

------------------------------------------------------------------------
-- NodeMap's (operations on nodes)
------------------------------------------------------------------------

data NodeData nodeLabel = NodeData {
   predecessors :: SmallSet Node,
   successors :: SmallSet Node,
   label :: nodeLabel
   }

defaultNodeData :: NodeData nodeLabel
defaultNodeData = NodeData {
   predecessors = emptySmallSet,
   successors = emptySmallSet,
   label = error "SharedGraph default unset"
   }

applyEdit :: Update Node nodeLabel -> NodeMap nodeLabel -> NodeMap nodeLabel
applyEdit 
      (EditNode{
         node=node,
         predecessorsOpt=predecessorsOpt,
         successorsOpt=successorsOpt,
         nodeLabelOpt=nodeLabelOpt
         })
      nodeMap1 =
   let
      nodeMap2 =
         case predecessorsOpt of
            Nothing -> nodeMap1
            Just predecessors ->
               setNodePredecessors node predecessors nodeMap1
      nodeMap3 =
         case successorsOpt of
            Nothing -> nodeMap2
            Just successors ->
               setNodeSuccessors node successors nodeMap2
      nodeMap4 =
         case nodeLabelOpt of
            Nothing -> nodeMap3
            Just nodeLabel -> 
               setNodeLabel node nodeLabel nodeMap3
   in
      nodeMap4   
        
 
setNodeLabel :: Node -> nodeLabel -> NodeMap nodeLabel -> NodeMap nodeLabel
setNodeLabel node nodeLabel nodeMap =
   let
      nodeData1 = lookupDefault nodeMap node
   in
      addNode nodeMap node (nodeData1{label=nodeLabel})

-- This function is exactly the same as setNodeSuccessors except
-- for interchange of "predecessors" with "successors"
setNodePredecessors :: Node -> SmallSet Node -> NodeMap nodeLabel -> 
   NodeMap nodeLabel
setNodePredecessors node predecessors2 nodeMap1 =
   let
      nodeData1 = lookupDefault nodeMap1 node
      predecessors1 = predecessors nodeData1
      nodeMap2 = addNode nodeMap1 node (nodeData1{predecessors=predecessors2})
      throwOut = minusSmallSet predecessors1 predecessors2
      nodeMap3 = 
         foldr
            (\ outNode nodeMap ->
               let
                  outNodeData = lookupDefault nodeMap outNode
               in
                  addNode nodeMap outNode (outNodeData{
                     successors=removeSmallSet node (successors outNodeData)
                     })
               )
            nodeMap2
            (listSmallSet throwOut)
      addIn = minusSmallSet predecessors2 predecessors1
      nodeMap4 = 
         foldr
            (\ inNode nodeMap ->
               let
                  inNodeData = lookupDefault nodeMap inNode
               in
                  addNode nodeMap inNode (inNodeData{
                     successors=addSmallSet node (successors inNodeData)
                     })
               )
            nodeMap3
            (listSmallSet throwOut)
   in
      nodeMap4    

-- This function is exactly the same as setNodePredecessors except
-- for interchange of "successors" with "predecessors"
setNodeSuccessors :: Node -> SmallSet Node -> NodeMap nodeLabel -> 
   NodeMap nodeLabel
setNodeSuccessors node successors2 nodeMap1 =
   let
      nodeData1 = lookupDefault nodeMap1 node
      successors1 = successors nodeData1
      nodeMap2 = addNode nodeMap1 node (nodeData1{successors=successors2})
      throwOut = minusSmallSet successors1 successors2
      nodeMap3 = 
         foldr
            (\ outNode nodeMap ->
               let
                  outNodeData = lookupDefault nodeMap outNode
               in
                  addNode nodeMap outNode (outNodeData{
                     predecessors=removeSmallSet node (predecessors outNodeData)
                     })
               )
            nodeMap2
            (listSmallSet throwOut)
      addIn = minusSmallSet successors2 successors1
      nodeMap4 = 
         foldr
            (\ inNode nodeMap ->
               let
                  inNodeData = lookupDefault nodeMap inNode
               in
                  addNode nodeMap inNode (inNodeData{
                     predecessors=addSmallSet node (predecessors inNodeData)
                     })
               )
            nodeMap3
            (listSmallSet throwOut)
   in
      nodeMap4    
   
------------------------------------------------------------------------
-- Instance of Graph
------------------------------------------------------------------------

instance Graph SharedGraph where
   getNodes (SharedGraph {nodeLookUp = nodeLookUp}) =
      do
         nodeMap <- readMVar nodeLookUp
         return (
            map
               (\ (node,nodeData) -> node)          
               (listNodeMap nodeMap)
            )
   
   getPredecessors sharedGraph node =
      do
         nodeData <- getNodeData sharedGraph node
         return (listSmallSet (predecessors nodeData))
   
   getSuccessors sharedGraph node =
      do
         nodeData <- getNodeData sharedGraph node
         return (listSmallSet (successors nodeData))
   
   getLabel sharedGraph node =
      do
         nodeData <- getNodeData sharedGraph node
         return (label nodeData)
   
   updateGraph = updateSharedGraph


getNodeData :: SharedGraph nodeLabel -> Node -> IO (NodeData nodeLabel)
getNodeData (SharedGraph {nodeLookUp = nodeLookUp}) node =
   do
      nodeMap <- readMVar nodeLookUp
      return(lookupError nodeMap node)
   



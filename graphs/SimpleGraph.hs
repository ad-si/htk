{- SimpleGraph is, as the name implies, a simple implementation
   of the Graph interface.  For example, we don't bother to sort
   the arcs going out of a node, meaning that to find out if two
   nodes are connected requires searching all the arcs out of one
   of the nodes, or all the arcs into the other.

   Notes on synchronicity.
      The Update operations Set*Label are intrinsically unsafe in
      this implementation since if two communicating SimpleGraphs
      both execute a Set*Label operation with different label values
      they may end up with each others values.  It is recommended that
      Set*Label only be used during the initialisation of the object,
      as a way of tieing the knot.

      In addition, Update operations which create a value based on a previous
      value (EG a NewNode creates a Node based on a NodeType), do
      assume that the previous value has already been created.

      I realise this is somewhat informal.  It may be necessary to
      replace SimpleGraph by something more complicated later . . .
   -}
module SimpleGraph(
   SimpleGraph -- implements Graph
   ) where

import List(delete)

import Computation (done)
import Object
import Registry

import Concurrent
import Selective
import BSem

import SIMClasses(Destructible(..))
import InfoBus

import Graph

------------------------------------------------------------------------
-- Data types and trivial instances
------------------------------------------------------------------------

data SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel =
   SimpleGraph {
      nodeData :: Registry Node (NodeData nodeLabel),
      nodeTypeData :: Registry NodeType nodeTypeLabel,
      arcData :: Registry Arc (ArcData arcLabel),
      arcTypeData :: Registry ArcType arcTypeLabel,
      clientsMVar :: MVar
         [ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel],
      parentDeRegister :: IO (),
         -- deRegister in GraphConnection from which graph was created.
      graphID :: ObjectID, 
         -- used to identify the graph (for InfoBus actually)
      bSem :: BSem -- All access operations should synchronize here.
      }

instance Synchronized 
      (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel) where
   synchronize graph command = synchronize (bSem graph) command

instance Object
      (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel) where
   objectID graph = graphID graph

data NodeData nodeLabel = NodeData {
   nodeLabel :: nodeLabel,
   nodeType :: NodeType,
   arcsIn :: [Arc],
   arcsOut :: [Arc]
   }

data ArcData arcLabel = ArcData {
   arcLabel :: arcLabel,
   arcType :: ArcType,
   source :: Node,
   target :: Node
   }

data ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel =
   ClientData {
      clientID :: ObjectID,
      clientSink :: (Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel 
         -> IO())
      }

instance Eq (ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
      where
   (==) clientData1 clientData2 = 
      (clientID clientData1) == (clientID clientData2)
   (/=) clientData1 clientData2 =
      (clientID clientData1) /= (clientID clientData2)

------------------------------------------------------------------------
-- Class Instance
------------------------------------------------------------------------

instance Graph SimpleGraph where
   getNodes graph = synchronize graph (listKeys (nodeData graph))

   getArcsOut = getNodeInfo arcsOut
   getArcsIn = getNodeInfo arcsIn
   getNodeLabel = getNodeInfo nodeLabel
   getNodeType = getNodeInfo nodeType

   getNodeTypeLabel graph nodeType =
      synchronize graph (getValue (nodeTypeData graph) nodeType)

   getSource = getArcInfo source
   getTarget = getArcInfo target
   getArcLabel = getArcInfo arcLabel
   getArcType = getArcInfo arcType

   getArcTypeLabel graph arcType =
      synchronize graph (getValue (arcTypeData graph) arcType)

   shareGraph graph =
      synchronize graph (
         do
            graphState <- cannGraph graph
            graphUpdatesQueue <- newMsgQueue
            let
               graphUpdates = receive graphUpdatesQueue
            clientID <- newObject 
            let
               clientSink update = sendIO graphUpdatesQueue update
               clientData = ClientData 
                  {clientID = clientID,clientSink = clientSink}
               mVar = clientsMVar graph
            oldClients <- takeMVar mVar
            putMVar mVar (clientData : oldClients)
            let
               deRegister =
                  do
                     -- It is intentional that we don't sync this.
                     -- I don't see how it can matter.
                     oldClients <- takeMVar mVar
                     putMVar mVar (delete clientData oldClients)
               graphUpdate update = 
                  applyUpdateFromClient graph update clientData

            return 
               (GraphConnection {
                  graphState = graphState,
                  graphUpdates = graphUpdates,
                  deRegister = deRegister,
                  graphUpdate = graphUpdate
                  })
         ) -- end of sync

   newGraph (GraphConnection {
      graphState = graphState,
      graphUpdates = graphUpdates,
      deRegister = deRegister,
      graphUpdate = graphUpdate
      }) =
      do
         graph <- uncannGraph graphState deRegister
         let
            mVar = clientsMVar graph
         -- modify client list  
         (oldClients@[]) <- takeMVar mVar
         -- if uncannGraph is later changed to add clients,
         -- we probably need to synchronize the changes to graph
         -- in this method!
         clientID <- newObject 
         let
            clientSink update = graphUpdate update
            clientData = ClientData 
               {clientID = clientID,clientSink = clientSink}
         putMVar mVar (clientData : oldClients)
         -- set up thread to listen to changes from parent.
         let
            receiveChanges =
               do
                  update <- sync graphUpdates
                  applyUpdateFromClient graph update clientData
                  receiveChanges
         forkIO receiveChanges
         -- register for destruction.
         registerTool graph
         return graph

   update graph update = applyUpdate graph update (const True)

getNodeInfo :: 
   (NodeData nodeLabel -> result)
   -> (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> Node
   -> IO result
getNodeInfo converter graph node =
   synchronize graph (
      do
         nodeData <- getValue (nodeData graph) node
         return (converter nodeData)
      )

getArcInfo :: 
   (ArcData arcLabel -> result)
   -> (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> Arc
   -> IO result
getArcInfo converter graph arc =
   synchronize graph (
      do
         arcData <- getValue (arcData graph) arc
         return (converter arcData)
      )

------------------------------------------------------------------------
-- We make it possible to destroy graphs.  It is not recommended
-- to destroy a graph before its children have been destroyed!
------------------------------------------------------------------------

instance Destructible
      (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel) where
   destroy graph =
      do
         deregisterTool graph
         synchronize graph (
            -- if anyone tries to access it afterwards, it will in
            -- fact be empty.
            do
               parentDeRegister graph
               -- Do a few things to encourage garbage collection.
               emptyRegistry (nodeData graph)
               emptyRegistry (nodeTypeData graph)
               emptyRegistry (arcData graph)
               emptyRegistry (arcTypeData graph)
               putMVar (clientsMVar graph) []
            ) -- end of synchronization

   destroyed graph =
      error "SimpleGraph: sorry, destructible event not implemented"  
               

------------------------------------------------------------------------
-- Updates
------------------------------------------------------------------------

applyUpdateFromClient ::
   SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> IO ()
applyUpdateFromClient graph update client =
   applyUpdate graph update 
      (\ clientToBroadcast -> client /= clientToBroadcast)

applyUpdate ::
   SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel 
   -> Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> (ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel -> Bool)
   -> IO ()
-- applyUpdate graph update proceedFn
--    updates graph with update.  It then broadcasts to all listeners
--    of the graph with classData such that proceedFn classData == True.
applyUpdate 
      (graph :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
      update proceedFn =
   synchronize graph (
      do
         -- (1) do the update.
         case update of
            NewNodeType nodeType nodeTypeLabel ->
               setValue (nodeTypeData graph) nodeType nodeTypeLabel
            SetNodeTypeLabel nodeType nodeTypeLabel ->
               setValue (nodeTypeData graph) nodeType nodeTypeLabel
            NewNode node nodeType nodeLabel ->
               setValue (nodeData graph) node
                  (NodeData {
                     nodeLabel = nodeLabel,
                     nodeType = nodeType,
                     arcsIn = [],
                     arcsOut = []
                     })
            DeleteNode node -> 
               do
                  let
                     nodeRegistry = nodeData graph
                     arcRegistry = arcData graph
                  (NodeData {arcsIn = arcsIn,arcsOut = arcsOut}
                        :: NodeData nodeLabel) <-
                     getValue nodeRegistry node
                  sequence_
                     (map
                        (\ arc -> deleteFromRegistry arcRegistry arc)
                        (arcsIn ++ arcsOut)
                        )
                  deleteFromRegistry (nodeData graph) node
            SetNodeLabel node nodeLabel ->
               do
                  let
                     registry = nodeData graph
                  (nodeData :: NodeData nodeLabel) <- getValue registry node
                  setValue registry node (nodeData {nodeLabel = nodeLabel})
            NewArcType arcType arcTypeLabel ->
               setValue (arcTypeData graph) arcType arcTypeLabel
            SetArcTypeLabel arcType arcTypeLabel ->
               setValue (arcTypeData graph) arcType arcTypeLabel
            NewArc arc arcType arcLabel nodeSource nodeTarget ->
               do
                  let
                     arcRegistry = arcData graph
                     nodeRegistry = nodeData graph
                     newArcData = ArcData {
                        arcLabel = arcLabel,
                        arcType = arcType,
                        source = nodeSource,
                        target = nodeTarget
                        }
                  setValue arcRegistry arc newArcData

                  (nodeSourceData :: NodeData nodeLabel)
                     <- getValue nodeRegistry nodeSource
                  let
                     newNodeSourceData = nodeSourceData {
                        arcsOut = arc : (arcsOut nodeSourceData)
                        }
                  setValue nodeRegistry nodeSource newNodeSourceData

                  (nodeTargetData :: NodeData nodeLabel) 
                     <- getValue nodeRegistry nodeTarget
                  let
                     newNodeTargetData = nodeTargetData {
                        arcsIn = arc : (arcsIn nodeTargetData)
                        }
                  setValue nodeRegistry nodeTarget newNodeTargetData
            DeleteArc arc ->
               do
                  let
                     arcRegistry = arcData graph
                     nodeRegistry = nodeData graph
                  (ArcData {source = source,target = target} 
                     :: ArcData arcLabel)
                     <- getValue arcRegistry arc
                  deleteFromRegistry arcRegistry arc

                  (nodeSourceData :: NodeData nodeLabel) 
                     <- getValue nodeRegistry source
                  let
                     newNodeSourceData = nodeSourceData {
                        arcsOut = delete arc (arcsOut nodeSourceData)
                        }
                  setValue nodeRegistry source newNodeSourceData

                  (nodeTargetData :: NodeData nodeLabel)
                     <- getValue nodeRegistry target
                  let
                     newNodeTargetData = nodeTargetData {
                        arcsIn = delete arc (arcsIn nodeTargetData)
                        }
                  setValue nodeRegistry target newNodeTargetData
            SetArcLabel arc arcLabel ->
               do
                  let
                     registry = arcData graph
                  (arcData :: ArcData arcLabel) <- getValue registry arc
                  setValue registry arc (arcData {arcLabel = arcLabel})
         -- (2) Tell the clients
         clients <- readMVar (clientsMVar graph)       
         sequence_
            (map
               (\ clientData ->
                  if proceedFn clientData
                     then
                        clientSink clientData update
                     else
                        done
                  )
               clients
               )
      ) -- end of synchronize.  

------------------------------------------------------------------------
-- Canning and Uncanning
-- These are the part of sharing graphs not involving communication.
------------------------------------------------------------------------

cannGraph :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel 
   -> IO (CannedGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
cannGraph (SimpleGraph{
   nodeData = nodeData,
   nodeTypeData = nodeTypeData,
   arcData = arcData,
   arcTypeData = arcTypeData
   }) =
   do
      nodeTypes <- listRegistryContents nodeTypeData
      arcTypes <- listRegistryContents arcTypeData

      nodeRegistryContents <- listRegistryContents nodeData
      let
         nodes =
            map
               (\ (node,NodeData 
                  {nodeLabel = nodeLabel,nodeType = nodeType}) ->
                  (node,nodeType,nodeLabel)
                  )
               nodeRegistryContents

      arcRegistryContents <- listRegistryContents arcData
      let
         arcs =
            map
               (\ (arc,ArcData
                  {arcType = arcType,arcLabel = arcLabel,
                     source = source,target = target}) ->
                  (arc,arcType,arcLabel,source,target)
                  )
               arcRegistryContents
      return (CannedGraph {nodeTypes = nodeTypes,nodes = nodes,
         arcTypes = arcTypes,arcs = arcs})
            
uncannGraph :: CannedGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> IO ()
   -> IO (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
-- the second argument is the deregistration function of the parent,
-- which we need to put in the SimpleGraph.
uncannGraph (CannedGraph {nodeTypes = nodeTypes,nodes = nodes,
   arcTypes = arcTypes,arcs = arcs}) parentDeRegister =
   do
      let
         nodeTypeUpdates =
            map 
               (\ (nodeType,nodeTypeLabel) -> 
                  NewNodeType nodeType nodeTypeLabel
                  )
               nodeTypes
         nodeUpdates =
            map
               (\ (node,nodeType,nodeLabel) ->
                  NewNode node nodeType nodeLabel
                  )
               nodes
         arcTypeUpdates =
            map
               (\ (arcType,arcTypeLabel) ->
                  NewArcType arcType arcTypeLabel
                  )
               arcTypes
         arcUpdates =
            map
               (\ (arc,arcType,arcLabel,source,target) ->
                  NewArc arc arcType arcLabel source target
                  )
               arcs

      nodeData <- newRegistry
      nodeTypeData <- newRegistry
      arcData <- newRegistry
      arcTypeData <- newRegistry
      clientsMVar <- newMVar []
      bSem <- newBSem
      graphID <- newObject

      let
         graph = SimpleGraph {
            nodeData = nodeData,nodeTypeData = nodeTypeData,
            arcData = arcData,arcTypeData = arcTypeData,
            parentDeRegister = parentDeRegister,
            clientsMVar = clientsMVar,
            bSem = bSem,
            graphID = graphID
            }
      sequence_ (
         map 
            (update graph) 
            (nodeTypeUpdates ++ nodeUpdates ++ arcTypeUpdates ++ arcUpdates)
         )
      return graph




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

import Concurrent
import Exception(tryAllIO)

import Computation (done)
import Object
import Registry

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
      (\ clientSink ->
         synchronize graph (
            do
               graphState <- cannGraph graph
               clientID <- newObject 
               let
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
                  (GraphConnectionData {
                     graphState = graphState,
                     deRegister = deRegister,
                     graphUpdate = graphUpdate
                     })
            ) -- end of sync
         )

   newGraph getGraphConnection =
      do
         graphUpdatesQueue <- newMsgQueue
         GraphConnectionData {
            graphState = graphState,
            deRegister = deRegister,
            graphUpdate = graphUpdate
            } <- getGraphConnection (sendIO graphUpdatesQueue)
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
                  update <- receiveIO graphUpdatesQueue
                  applyUpdateFromClient graph update clientData
                  receiveChanges
         forkIO receiveChanges
         -- register for destruction.
         registerTool graph
         return graph

   update graph update = applyUpdate graph update (const True)

   newEmptyGraph =
      do
         nodeData <- newRegistry
         nodeTypeData <- newRegistry
         arcData <- newRegistry
         arcTypeData <- newRegistry
         clientsMVar <- newMVar []
         bSem <- newBSem
         graphID <- newObject
         return (SimpleGraph {
            nodeData = nodeData,nodeTypeData = nodeTypeData,
            arcData = arcData,arcTypeData = arcTypeData,
            parentDeRegister = done,
            clientsMVar = clientsMVar,
            bSem = bSem,
            graphID = graphID
            })

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
applyUpdate graph update proceedFn =
   do
      -- (1) Update graph, and get list of current clients.
      clients <- synchronize graph (innerApplyUpdate graph update)
      -- (2) Tell the clients
      sequence_
         (map
            (\ clientData ->
               if proceedFn clientData
                  then
                     do
                        result <- tryAllIO (clientSink clientData update) 
                        case result of
                           Left exception ->
                              putStrLn ("Client error "++(show exception))
                           Right () -> done
                  else
                     done
               )
            clients
            )

innerApplyUpdate ::
   SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel 
   -> Update nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> IO [ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel]
innerApplyUpdate 
      (graph :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
      update =
   let
      passOnUpdate = readMVar (clientsMVar graph)
      killUpdate = return []
      arcRegistry = arcData graph
      nodeRegistry = nodeData graph
   in
      -- The following cases need special treatment, in case
      -- someone else has deleted a relevant Node or Arc before
      -- we get there: SetNodeLabel, SetArcLabel, DeleteNode, DeleteArc.
      -- In these cases we (a) do nothing;
      -- (b) return a null client list, to prevent the update
      -- being passed on to anyone else.
      -- We don't give 
      case update of
         NewNodeType nodeType nodeTypeLabel ->
            do
               setValue (nodeTypeData graph) nodeType nodeTypeLabel
               passOnUpdate
         SetNodeTypeLabel nodeType nodeTypeLabel ->
            do
               setValue (nodeTypeData graph) nodeType nodeTypeLabel
               passOnUpdate
         NewNode node nodeType nodeLabel ->
            do
               setValue (nodeData graph) node
                  (NodeData {
                     nodeLabel = nodeLabel,
                     nodeType = nodeType,
                     arcsIn = [],
                     arcsOut = []
                     })
               passOnUpdate
         DeleteNode node -> 
            do
               nodeDataOpt <- getValueOpt nodeRegistry node
               case nodeDataOpt of
                  Nothing -> killUpdate
                  Just (NodeData {arcsIn = arcsIn,arcsOut = arcsOut}
                     :: NodeData nodeLabel) ->
                     do
                        sequence_
                           (map
                              (\ arc -> 
                                 innerApplyUpdate graph (DeleteArc arc))
                              (arcsIn ++ arcsOut)
                              )
                        deleteFromRegistry nodeRegistry node
                        passOnUpdate
         SetNodeLabel node nodeLabel ->
            do
               nodeDataOpt <- getValueOpt nodeRegistry node
               case nodeDataOpt of
                  Nothing -> killUpdate
                  Just (nodeData :: NodeData nodeLabel) -> 
                     do
                        setValue nodeRegistry node 
                           (nodeData {nodeLabel = nodeLabel})
                        passOnUpdate
         NewArcType arcType arcTypeLabel ->
            do
               setValue (arcTypeData graph) arcType arcTypeLabel
               passOnUpdate
         SetArcTypeLabel arcType arcTypeLabel ->
            do
               setValue (arcTypeData graph) arcType arcTypeLabel
               passOnUpdate
         NewArc arc arcType arcLabel nodeSource nodeTarget ->
            do
               nodeSourceDataOpt <- getValueOpt nodeRegistry nodeSource
               nodeTargetDataOpt <- getValueOpt nodeRegistry nodeTarget
               case (nodeSourceDataOpt,nodeTargetDataOpt) of
                  (Just (nodeSourceData :: NodeData nodeLabel),
                   Just (nodeTargetData :: NodeData nodeLabel)) ->
                     do
                        let
                           newArcData = ArcData {
                              arcLabel = arcLabel,
                              arcType = arcType,
                              source = nodeSource,
                              target = nodeTarget
                              }
                        setValue arcRegistry arc newArcData
                        if (nodeSource == nodeTarget) 
                           then
                              do
                                 let
                                    newNodeSourceData = nodeSourceData {
                                       arcsOut = 
                                          arc : arc : (arcsOut nodeSourceData)
                                       }
                                 setValue nodeRegistry nodeSource 
                                    newNodeSourceData
                           else
                              do
                                 let
                                    newNodeSourceData = nodeSourceData {
                                       arcsOut = arc : 
                                          (arcsOut nodeSourceData)
                                       }
                                    newNodeTargetData = nodeTargetData {
                                       arcsIn = arc : (arcsIn nodeTargetData)
                                       }
                                 setValue nodeRegistry nodeSource 
                                    newNodeSourceData
                                 setValue nodeRegistry nodeTarget 
                                    newNodeTargetData
                        passOnUpdate
                  _ -> killUpdate
         DeleteArc arc ->
            do
               arcDataOpt <- getValueOpt arcRegistry arc
               case arcDataOpt of
                  Nothing -> killUpdate
                  Just (ArcData {source = source,target = target} 
                     :: ArcData arcLabel) ->
                     do
                        -- The getValue operations for the source and
                        -- target must succeed, because if the arc is
                        -- still there, the nodes must also still be there.
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
                        passOnUpdate
         SetArcLabel arc arcLabel ->
            do
               arcDataOpt <- getValueOpt arcRegistry arc
               case arcDataOpt of
                  Just (arcData :: ArcData arcLabel) ->
                     do
                        setValue arcRegistry arc 
                           (arcData {arcLabel = arcLabel})
                        passOnUpdate
                  Nothing -> killUpdate

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
      nodeRegistryContents <- listRegistryContents nodeData
      arcTypes <- listRegistryContents arcTypeData
      arcRegistryContents <- listRegistryContents arcData
  
      let
         nodeTypeUpdates =
            map
               (\ (nodeType,nodeTypeLabel) 
                  -> NewNodeType nodeType nodeTypeLabel
                  )
               nodeTypes
         nodeUpdates =
            map
               (\ (node,NodeData {nodeType = nodeType,nodeLabel = nodeLabel})
                  -> NewNode node nodeType nodeLabel
                  )
               nodeRegistryContents 
         arcTypeUpdates =
            map
               (\ (arcType,arcTypeLabel) 
                  -> NewArcType arcType arcTypeLabel
                  )
               arcTypes
         arcUpdates =
            map
               (\ (arc,ArcData {arcType = arcType,arcLabel = arcLabel,
                     source = source,target = target}) 
                  -> NewArc arc arcType arcLabel source target
                  )
               arcRegistryContents

      return (CannedGraph {
         updates = nodeTypeUpdates ++ nodeUpdates ++ arcTypeUpdates 
            ++ arcUpdates
            })
            
uncannGraph :: CannedGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> IO ()
   -> IO (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
-- the second argument is the deregistration function of the parent,
-- which we need to put in the SimpleGraph.
uncannGraph 
      ((CannedGraph {updates = updates}) 
         :: CannedGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel) 
      parentDeRegister =
   do
      (graph' :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel) 
         <- newEmptyGraph
      let
         graph = graph {parentDeRegister = parentDeRegister}
      sequence_ (map (update graph) updates)
      return graph




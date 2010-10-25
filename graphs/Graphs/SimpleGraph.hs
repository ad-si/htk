{-# LANGUAGE ScopedTypeVariables #-}

-- | SimpleGraph is, as the name implies, a simple implementation
-- of the Graph interface.  For example, we don't bother to sort
-- the arcs going out of a node, meaning that to find out if two
-- nodes are connected requires searching all the arcs out of one
-- of the nodes, or all the arcs into the other.
--
-- Notes on synchronicity.
--    The Update operations Set*Label are intrinsically unsafe in
--    this implementation since if two communicating SimpleGraphs
--    both execute a Set*Label operation with different label values
--    they may end up with each others values.  It is recommended that
--    Set*Label only be used during the initialisation of the object,
--    as a way of tieing the knot.
--
--    In addition, Update operations which create a value based on a previous
--    value (EG a NewNode creates a Node based on a NodeType), do
--    assume that the previous value has already been created.
--
--    I realise this is somewhat informal.  It may be necessary to
--    replace SimpleGraph by something more complicated later . . .
module Graphs.SimpleGraph(
   SimpleGraph, -- implements Graph

   getNameSource,
   -- :: SimpleGraph -> NameSource
   -- We need to hack the name source as part of the backup process.


   delayedAction,
      -- :: Graph graph
      -- => graph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
      -- -> Node -- node
      -- -> IO () -- action to perform when the node is created in the graph.
      -- -> IO ()

   ClientData(..),

   ) where

import Data.List(delete)

import Control.Concurrent
import Control.Exception

import Util.Computation (done)
import Util.Object
import Util.Registry
import Util.AtomString

import Events.Destructible
import Events.Events
import Events.Channels
import Events.Synchronized

import Reactor.BSem

import Reactor.InfoBus

import Graphs.NewNames
import Graphs.Graph

------------------------------------------------------------------------
-- Data types and trivial instances
------------------------------------------------------------------------

data SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel =
   SimpleGraph {
      nodeData :: Registry Node (NodeData nodeLabel),
      nodeTypeData :: Registry NodeType nodeTypeLabel,
      arcData :: Registry Arc (ArcData arcLabel),
      arcTypeData :: Registry ArcType arcTypeLabel,
      nameSource :: NameSource,
         -- Where new Node/Arc/NodeType/ArcType's can come from.
      clientsMVar :: MVar
         [ClientData nodeLabel nodeTypeLabel arcLabel arcTypeLabel],
      parentDeRegister :: IO (),
         -- deRegister in GraphConnection from which graph was created.
      graphID :: ObjectID,
         -- used to identify the graph (for InfoBus actually)
      bSem :: BSem -- All access operations should synchronize here.
      }

getNameSource :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel ->
   NameSource
getNameSource simpleGraph = nameSource simpleGraph

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
   getArcs graph = synchronize graph (listKeys (arcData graph))
   getNodeTypes graph = synchronize graph (listKeys (nodeTypeData graph))
   getArcTypes graph = synchronize graph (listKeys (arcTypeData graph))

   getArcsOut = getNodeInfo arcsOut
   getArcsIn = getNodeInfo arcsIn
   getNodeLabel = getNodeInfo nodeLabel
   getNodeType = getNodeInfo nodeType

   getNodeTypeLabel graph nodeType =
      synchronize graph (
         getValue' "NodeTypeLabel" (nodeTypeData graph) nodeType)

   getSource = getArcInfo source
   getTarget = getArcInfo target
   getArcLabel = getArcInfo arcLabel
   getArcType = getArcInfo arcType

   getArcTypeLabel graph arcType =
      synchronize graph (
         getValue' "ArcTypeLabel" (arcTypeData graph) arcType)

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

               nameSourceBranch <- branch (nameSource graph)

               return
                  (GraphConnectionData {
                     graphState = graphState,
                     deRegister = deRegister,
                     graphUpdate = graphUpdate,
                     nameSourceBranch = nameSourceBranch
                     })
            ) -- end of sync
         )

   newGraph getGraphConnection =
      do
         graphUpdatesQueue <- newChannel
         GraphConnectionData {
            graphState = graphState,
            deRegister = deRegister,
            graphUpdate = graphUpdate,
            nameSourceBranch = nameSourceBranch
            } <- getGraphConnection (sync . noWait . (send graphUpdatesQueue))

         graph <- uncannGraph graphState deRegister nameSourceBranch
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

   newNodeType graph nodeTypeLabel =
      do
         name <- getNewName (nameSource graph)
         let (nodeType :: NodeType) = fromString name
         update graph (NewNodeType nodeType nodeTypeLabel)
         return nodeType

   newNode graph nodeType nodeLabel =
      do
         name <- getNewName (nameSource graph)
         let (node :: Node) = fromString name
         update graph (NewNode node nodeType nodeLabel)
         return node

   newArcType graph arcTypeLabel =
      do
         name <- getNewName (nameSource graph)
         let (arcType :: ArcType) = fromString name
         update graph (NewArcType arcType arcTypeLabel)
         return arcType

   newArc graph arcType arcLabel source target =
      do
         name <- getNewName (nameSource graph)
         let (arc :: Arc) = fromString name
         update graph (NewArc arc arcType arcLabel source target)
         return arc

   update graph update = applyUpdate graph update (const True)

   newEmptyGraph = newEmptyGraphWithSource initialBranch

getNodeInfo ::
   (NodeData nodeLabel -> result)
   -> (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
   -> Node
   -> IO result
getNodeInfo converter graph node =
   synchronize graph (
      do
         (Just nodeData) <- getValueOpt (nodeData graph) node
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
         (Just arcData) <- getValueOpt (arcData graph) arc
         return (converter arcData)
      )

------------------------------------------------------------------------
-- We make it possible to destroy graphs.  It is not recommended
-- to destroy a graph before its children have been destroyed!
------------------------------------------------------------------------

instance Destroyable
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
               let
                  mVar = clientsMVar graph
               takeMVar mVar
               putMVar mVar []
            ) -- end of synchronization

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
   synchronize graph (
      do
         -- (1) Update graph, and get list of current clients.
         clients <- innerApplyUpdate graph update
         -- (2) Tell the clients
         sequence_
            (map
               (\ clientData ->
                  if proceedFn clientData
                     then
                        do
                           result <- Control.Exception.try
                              (clientSink clientData update)
                           case result of
                              Left exception ->
                                 putStrLn ("Client error " ++
                                           show (exception :: SomeException))
                              Right () -> done
                     else
                        done
                  )
               clients
               )
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
         SetNodeType node nodeType ->
            do
               nodeDataOpt <- getValueOpt nodeRegistry node
               case nodeDataOpt of
                  Nothing -> killUpdate
                  Just (nodeData :: NodeData nodeLabel) ->
                     do
                        setValue nodeRegistry node
                           (nodeData {nodeType = nodeType})
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
                           <- getValue' "DeleteArc" nodeRegistry target
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
         SetArcType arc arcType ->
            do
               arcDataOpt <- getValueOpt arcRegistry arc
               case arcDataOpt of
                  Just (arcData :: ArcData arcLabel) ->
                     do
                        setValue arcRegistry arc
                           (arcData {arcType = arcType})
                        passOnUpdate
                  Nothing -> killUpdate
         MultiUpdate updates ->
            do
               mapM_ (innerApplyUpdate graph) updates
               passOnUpdate

------------------------------------------------------------------------
-- Canning, Uncanning, and graph creation.
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
   -> IO () -> NameSourceBranch
   -> IO (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
-- the second argument is the deregistration function of the parent,
-- which we need to put in the SimpleGraph.  The third argument is
-- the graph's NameSource, ditto.
uncannGraph
      ((CannedGraph {updates = updates})
         :: CannedGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
      parentDeRegister nameSourceBranch =
   do
      (graph' :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
         <- newEmptyGraphWithSource nameSourceBranch
      let
         graph = graph' {parentDeRegister = parentDeRegister}
      sequence_ (map (update graph) updates)
      return graph

newEmptyGraphWithSource :: NameSourceBranch
   -> IO (SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
newEmptyGraphWithSource nameSourceBranch =
   do
      nodeData <- newRegistry
      nodeTypeData <- newRegistry
      arcData <- newRegistry
      arcTypeData <- newRegistry
      clientsMVar <- newMVar []
      bSem <- newBSem
      graphID <- newObject
      nameSource <- useBranch nameSourceBranch

      return (SimpleGraph {
         nodeData = nodeData,nodeTypeData = nodeTypeData,
         arcData = arcData,arcTypeData = arcTypeData,
         nameSource = nameSource,
         parentDeRegister = done,
         clientsMVar = clientsMVar,
         bSem = bSem,
         graphID = graphID
         })



------------------------------------------------------------------------
-- delayedAction is used to delay an action until a node is present.
-- It assumes that the node is not already present.
-- Typical use: register that an arc shall be added when a given node
-- at one end of the arc is created.
------------------------------------------------------------------------

delayedAction ::
   SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel
   -> Node -- node
   -> IO () -- action to perform when the node is created in the graph.
   -> IO ()
delayedAction
      (graph :: SimpleGraph nodeLabel nodeTypeLabel arcLabel arcTypeLabel)
      node action =
   do
      doNow <- transformValue (nodeData graph) node
         (\ (nodeDataOpt :: Maybe (NodeData nodeLabel)) ->
            case nodeDataOpt of
               Just nodeData -> return (nodeDataOpt,True)
               Nothing ->
                  do
                     -- we create a new client for the purpose.
                     clientID <- newObject
                     let
                        clients = clientsMVar graph

                        clientData = ClientData {
                           clientID = clientID,clientSink = clientSink
                           }

                        clientSink update = case update of
                           NewNode node1 _ _
                              | node1 == node
                              ->
                                 do
                                    forkIO action
                                    modifyMVar_ clients
                                       (return . delete clientData)
                           _ -> done

                     modifyMVar_ clients (return . (clientData :))
                     return (nodeDataOpt,False)
            )
      if doNow then action else done



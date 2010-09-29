-- | GraphEditorRemote sets up a graph editor attached to a remote server.
module Graphs.GraphEditorRemote(graphEditorRemote) where

import Control.Concurrent

import Util.Computation(done)
import Util.BinaryAll

import Util.Thread
import Server.HostsPorts

import Server.CallServer
import Events.Destructible
import Events.Events

import Graphs.Graph as Graph
import Graphs.GraphDisp as GraphDisp
import Graphs.GraphConfigure as GraphConfigure
import Graphs.GraphEditor
import Graphs.GraphEditorService
import Graphs.SimpleGraph

graphEditorRemote :: -- does not return until editor is closed.
   (?server :: HostPort,
      GraphConfigure.GraphAllConfig dispGraph graphParms
         node nodeType nodeTypeParms arc arcType arcTypeParms,
    HasConfigValue Shape nodeTypeParms)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> IO ()
graphEditorRemote displaySort =
   do
      (updateServer,getUpdate,closeConnection,initialState) <-
         connectBroadcastOther graphEditorService
      let
         FrozenGraph {
            graphState' = graphState,
            nameSourceBranch' = nameSourceBranch
            } = read initialState

      (updateThreadMVar :: MVar ThreadId) <- newEmptyMVar
      let
         graphConnection updateSink =
            do
               let
                  graphConnectionData = GraphConnectionData {
                     graphState = graphState,
                     deRegister = done,
                     graphUpdate = updateServer . ReadShow ,
                     nameSourceBranch = nameSourceBranch
                     }
                  listenToServer =
                     do
                        (ReadShow newUpdate) <- getUpdate
                        updateSink newUpdate
                        listenToServer
               updateThread <- forkIO(goesQuietly listenToServer)
               putMVar updateThreadMVar updateThread
               return graphConnectionData

      (graph :: Displayable SimpleGraph) <-
         Graph.newGraph graphConnection
      graphEditor <- newGraphEditor displaySort graph
      sync(destroyed graphEditor)
      closeConnection
      updateThread <- takeMVar updateThreadMVar
      killThread updateThread







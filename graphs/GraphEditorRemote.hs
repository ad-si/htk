{- GraphEditorRemote sets up a graph editor attached to a remote server. 
   -}
module GraphEditorRemote(graphEditorRemote) where

import Concurrent

import Computation(done)
import BinaryIO

import Thread
import HostsPorts

import CallServer
import Destructible
import Events

import Graph
import GraphDisp
import GraphConfigure
import GraphEditor
import GraphEditorService
import SimpleGraph

graphEditorRemote :: -- does not return until editor is closed.
   (GraphConfigure.GraphAllConfig dispGraph graphParms 
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

      (updateThreadMVar :: MVar ThreadID) <- newEmptyMVar
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







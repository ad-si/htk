{- GraphEditorRemote sets up a graph editor attached to a remote server. 
   We use the now fairly standard port 11393 . . .. -}
module GraphEditorRemote(graphEditorRemote) where

import Concurrent(killThread)

import Computation(done)

import Concurrency
import Thread
import SocketEV

import CallServer
import SIM(destroyed,sync)

import Graph
import GraphDisp
import GraphConfigure
import GraphEditor
import GraphEditorService
import SimpleGraph

graphEditorRemote :: -- does not return until editor is closed.
   (GraphConfigure.GraphAllConfig dispGraph graphParms 
      node nodeType nodeTypeParms arc arcType arcTypeParms,
    HasConfigValue Shape nodeTypeParms,
    DescribesHost server
    )
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> server
   -> IO ()
graphEditorRemote displaySort server =
   do
      (updateServer,getUpdate,closeConnection,initialState) <- 
         connectBroadcastOther graphEditorService server (11393::Int)
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
                     graphUpdate = updateServer,
                     nameSourceBranch = nameSourceBranch
                     }
                  listenToServer =
                     do
                        newUpdate <- getUpdate
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







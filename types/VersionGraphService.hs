{- This provides a ServiceClass instance for the Version graph.
   It is fairly similar to GraphEditorService, except for the types.
   Specifically
      nodes are labelled with String.
      arcs are labelled with ().
      node & arc types are labelled with ().

   We store important information in the Strings inside the Node and Arc
   values.  For the format of this, see the file VersionGraph.hs.
   -}
module VersionGraphService(
   versionGraphService, -- :: pass to connectBroadcastOther to call server
   versionGraphServiceWrapped, -- pass to server
   FrozenGraph(..), -- What is sent (Showed) on client connect.
   VersionTypes, -- abbreviations for the type of things.

   -- The following types are in fact initialised by the Initialisation
   -- module
   checkedInType,workingType, -- :: NodeType
      -- These are types of nodes in the graph.  As a matter of fact,
      -- all the nodes the server sees will have type "checkedInType",
      -- but we provide workingType as well, and both types are part of the
      -- graph.
   checkedInArcType,workingArcType, -- :: ArcType
      -- Similar to checkedInType and workingType; a checkedInArcType should
      -- only bind two checkedInType nodes.
   ) where

import IO

import AtomString
import Computation
import IOExtras
import BinaryIO
import WBFiles

import Thread(secs)

import ServiceClass

import NewNames
import Graph
import SimpleGraph

import View(Version)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type VersionTypes dataSort = dataSort String () () () 
   -- change to alter node/nodetype/arc/arctypes.
   -- The NodeType uses as String the string representation of
   -- the version.  Thus each node has attached to it the NodeType
   -- plus the additional String which is supplied; this String
   -- can be used to provide additional information about the version.

type VersionGraph = VersionTypes SimpleGraph

type VersionUpdate = VersionTypes Update

type CannedVersionGraph = VersionTypes CannedGraph

data FrozenGraph = FrozenGraph {
   graphState' :: CannedVersionGraph,
   nameSourceBranch' :: NameSourceBranch
   } deriving (Read,Show) -- sent on branching

versionGraphServiceWrapped = Service versionGraphService

versionGraphService = serviceArg 
   :: (ReadShow VersionUpdate,ReadShow VersionUpdate,VersionGraphDB)


------------------------------------------------------------------------------
-- The pre-defined types.  Clients aren't supposed to define any others,
-- at least not yet.
-- Only the types checkedInType and checkedInTypeNode should occur in
-- the copy of the graph in the server.  The other types are used to represent
-- working nodes used by the client (see VersionGraph.hs)
-- An arc with type checkedInArcType should only connect two nodes with
-- type checkedInType.
------------------------------------------------------------------------------

checkedInType :: NodeType
checkedInType = fromString "C"

workingType :: NodeType
workingType = fromString "W"

checkedInArcType :: ArcType
checkedInArcType = fromString "C"

workingArcType :: ArcType
workingArcType = fromString "W"

------------------------------------------------------------------------------
-- The instance
------------------------------------------------------------------------------

instance ServiceClass (ReadShow VersionUpdate) (ReadShow VersionUpdate) 
      VersionGraphDB where

   serviceId _ = "VersionGraph"

   serviceMode _ = BroadcastOther

   handleRequest _ _ (ReadShow newUpdate,versionGraphDB) =
      do
         updateVersionGraphDB versionGraphDB newUpdate
         return (ReadShow newUpdate,versionGraphDB)

   getBackupDelay _ = return BackupNever

   sendOnConnect _ _ (versionGraphDB @ (VersionGraphDB {graph = graph})) =
      do
         recordBranch versionGraphDB

         let
             graphConnection = shareGraph graph
             newUpdate _ = error "Update in mid-sendOnConnect!"
         -- shouldn't happen because we deregister updates
         -- and handleRequest won't be called again until sendOnConnect
         -- is finished.
         GraphConnectionData {
            graphState = graphState',
            deRegister = deRegister,
            nameSourceBranch = nameSourceBranch'
            } <- graphConnection newUpdate
         deRegister
         return (show (FrozenGraph {
            graphState' = graphState',
            nameSourceBranch' = nameSourceBranch'
            }))
   
   initialStateFromString _ _ = newVersionGraphDB 

------------------------------------------------------------------------------
-- The backup file.  (We do not use the standard backup procedure.)
------------------------------------------------------------------------------

data VersionGraphDB = VersionGraphDB {
   graph :: VersionGraph,
   backupFile :: Handle
   }

newVersionGraphDB :: IO VersionGraphDB 
newVersionGraphDB =
   do
      -- (1) Create the graph
      graph <- newEmptyGraph

      -- (2) Read in the updates from the backup file.
      fpath <- getServerFile "versionGraph"
      handle <- openFile fpath ReadWriteMode
      readUpdates handle graph

      let
         versionGraphDB = VersionGraphDB {
            graph = graph,
            backupFile = handle
            }
      return versionGraphDB

-- This type records the things we put in the update file.
data GraphUpdate = 
      VersionUpdate VersionUpdate -- topological changes
   |  Branch 
         -- record a new client branch.  We need to do this so that
         -- names get uniquely allocated.
   deriving (Read,Show)


updateVersionGraphDB :: VersionGraphDB -> VersionUpdate -> IO ()
updateVersionGraphDB (VersionGraphDB {graph = graph,backupFile = handle}) 
      update1 =
   do
      hPut handle (ReadShow (VersionUpdate update1))
      hFlush handle
      update graph update1

recordBranch :: VersionGraphDB -> IO ()
recordBranch (VersionGraphDB {backupFile = handle}) =
   do
      hPut handle (ReadShow Branch)
      hFlush handle

readUpdates :: Handle -> VersionGraph -> IO ()
readUpdates handle graph =
   -- based on SimpleDBServer.readFrozenVersionDatas
   do
      let
         nameSource = getNameSource graph

      pos1 <- hGetPosn handle
      updateWEOpt <- catchEOF (hGetWE handle)
      case updateWEOpt of
         Nothing -> -- EOF
            do
               pos2 <- hGetPosn handle
               unless (pos1 == pos2)
                  (do 
                     putStrLn 
                        "Restarting server: incomplete commit discarded"
                     hSetPosn pos1
                  )
               done -- this is how we normally end.
         Just updateWE -> 
            case fromWithError updateWE of
               Left mess ->
                  error (
                     "Server could not be restarted due to error reading "
                     ++ "version graph file: " ++ mess)
               Right (ReadShow (VersionUpdate update1)) ->
                  do
                     update graph update1
                     readUpdates handle graph
               Right (ReadShow Branch) ->
                  do
                     branch nameSource
                     readUpdates handle graph
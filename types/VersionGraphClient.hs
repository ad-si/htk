-- | This module contains the code which connects to the VersionInfo service
-- (defined in simpledb) and constructs a graph.  It also defines 
-- functions for packing and unpacking Node values. 
module VersionGraphClient(
   -- Get the VersionGraph, and arrange for the connection to be closed
   -- when the program ends.

   -- types
   VersionGraphClient, -- contains the graph
   VersionGraphNode(..), -- represents a node in the graph 
   VersionInfo1, -- generalised VersionInfo that can also contain the view.

   mkVersionGraphClient, -- :: (?server :: HostPort) => IO VersionGraphClient
   mkVersionGraphClientInternal, -- :: VersionState -> IO VersionGraphClient

   -- operations for modifying the graph.
   -- NB.  Changes to the committed versions are not given by this functions,
   -- but come from the server.
   newWorkingVersion, 
      -- :: VersionGraphClient -> View 
      -- -> IO VersionGraphNode
      -- The parents are changed, and updated, from the view's 
      -- viewInfoBroadcaster
   deleteWorkingVersion,
      -- :: VersionGraphClient -> View -> IO ()


   setNewFilter0, -- :: VersionGraphClient -> VersionGraphFilter -> IO ()

   toVersionGraphConnection,  
      -- :: VersionGraphClient -> GraphConnection VersionInfo1 () () ()
      -- How to draw the graph.

   VersionTypes,
   getInputGraphBack,


   toVersionInfo, -- :: VersionInfo1 -> VersionInfo
   toViewOpt, -- :: VersionInfo1 -> Maybe View
      -- How to take apart what you get from toversionGraphConnection

   getVersionInfo,
      -- :: VersionGraphClient -> ObjectVersion -> (IO VersionInfo)
   getVersionInfos,
      -- :: VersionGraphClient -> IO [VersionInfo1]

   -- node and arc types
   checkedInType, workingType, checkedInTypeHidden, workingTypeHidden,
      -- :: NodeType
   checkedInArcType, workingArcType, arcTypeHidden, 
      -- :: ArcType
   ) where

import Maybe

import Control.Concurrent.MVar

import AtomString
import Thread
import Sources
import Sink
import Dynamics
import ExtendedPrelude

import InfoBus

import HostsPorts hiding (user)
import CallServer

import FindCommonParents(GraphBack)
import Graph
import GraphConnection
import VersionDag hiding (changeIsHidden,getInputGraphBack)
import qualified VersionDag

import VersionState(VersionState,registerAct)
import qualified VersionState

import VersionDB hiding (VersionInfo1)
import ViewType
import VersionInfo
import VersionInfoFilter

import VersionInfoService

-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

data VersionGraphNode = 
      CheckedInNode ObjectVersion
   |  WorkingNode View
   deriving (Eq,Ord)

data VersionInfo1 = VersionInfo1 {
   versionInfo :: VersionInfo,
   viewOpt :: Maybe View
   } deriving (Typeable)

-- use data declaration to work around ghc6.2.1 bug.
data VersionGraphClient = VersionGraphClient {
   versionDag :: VersionDag VersionGraphNode VersionInfo1 Bool
      -- ^ the Bool is True for a checked-in version
   }
 
type VersionTypes dataSort = dataSort VersionInfo1 () Bool ()

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance Show VersionGraphNode where -- this is mainly used for debugging
   show (CheckedInNode ov) = "Checked in node:" ++ show ov
   show (WorkingNode _) = "Unknown view" 


versionInfo1Map :: VersionInfo1 -> (Full VersionInfo,Maybe View)
versionInfo1Map versionInfo1 
   = (Full (versionInfo versionInfo1),viewOpt versionInfo1)

instance Eq VersionInfo1 where
   (==) = mapEq versionInfo1Map

instance Ord VersionInfo1 where
   compare = mapOrd versionInfo1Map   

-- ------------------------------------------------------------------------
-- Creating VersionGraphClient's.
-- ------------------------------------------------------------------------

-- | Get the VersionGraph, and arrange for the connection to be closed
-- when the program ends.
mkVersionGraphClient :: (?server :: HostPort) => IO VersionGraphClient
mkVersionGraphClient =
   do
      (versionGraph,terminator) <- connectToServer
      registerDestroyAct terminator
      return versionGraph

-- | Get the VersionGraph (internal version), and arrange for the connection to
-- be closed when the program ends.
mkVersionGraphClientInternal :: VersionState -> IO VersionGraphClient
mkVersionGraphClientInternal versionState =
   do
      (versionGraph,terminator) <- connectToServerInternal versionState
      registerDestroyAct terminator
      return versionGraph

-- | connectToServer generates a new graph connected to the version
-- graph in the server.  Updates to this graph do not get passed to
-- the server.
-- The returned action closes the server connection.
connectToServer :: (?server :: HostPort) => IO (VersionGraphClient,IO ())
connectToServer =
   do
      (getNextUpdate,closeConnection,initialVersionInfos) 
          <- connectExternal versionInfoService
      connectToServer1 getNextUpdate closeConnection initialVersionInfos

-- | connectToServerInternal connects to the internal server.
connectToServerInternal :: VersionState -> IO (VersionGraphClient,IO ())
connectToServerInternal versionState =
   do
      (updateMVar :: MVar (Bool,VersionInfo)) <- newEmptyMVar
      let
         updateFn updateAct =
            do
               update <- updateAct
               putMVar updateMVar update

      registerAct versionState updateFn

      versionInfos <- VersionState.getVersionInfos versionState
      let
         getNextUpdate = takeMVar updateMVar

      connectToServer1 getNextUpdate done versionInfos    

-- | This is the general version-graph construction function, for
-- external and internal version graphs.  
connectToServer1 :: 
   IO (Bool,VersionInfo) 
       -- ^ source of updates.  The Bool indicates that a version info is
       -- not new, but simply being changed.
   -> IO () -- ^ close action
   -> [VersionInfo] -- ^ initial versionInfos
   -> IO (VersionGraphClient,IO ())
connectToServer1 getNextUpdate closeConnection initialVersionInfos =
   do
      let
         toNodeKey :: VersionInfo1 -> VersionGraphNode
         toNodeKey versionInfo1 = case viewOpt versionInfo1 of
            Just view -> WorkingNode view
            Nothing -> CheckedInNode (
               version . user . versionInfo $ versionInfo1)

         toParents :: VersionInfo1 -> [(Bool,VersionGraphNode)]
         toParents versionInfo1 =
            let
               isWorking = isJust . viewOpt $ versionInfo1 
            in
               map 
                  (\ parent -> (isWorking,CheckedInNode parent))
                  (parents . user . versionInfo $ versionInfo1) 

      versionDag <- newVersionDag 
         ((mkHidden defaultVersionInfoFilter) . versionInfo) 
         toNodeKey toParents

      let
         mkVersionInfoFromServer :: VersionInfo -> VersionInfo1
         mkVersionInfoFromServer versionInfo =
            VersionInfo1 {
               versionInfo = versionInfo,
               viewOpt = Nothing
               }

      addVersions versionDag (map mkVersionInfoFromServer initialVersionInfos)

      let
         versionGraphClient = VersionGraphClient {
            versionDag = versionDag
            }


      let
         newVersionsThread =
            do
               (isChange,versionInfo) <- getNextUpdate
               (if isChange then setNodeInfo else addVersion) versionDag
                  (mkVersionInfoFromServer versionInfo)

               newVersionsThread

      forkIODebug newVersionsThread

      return (versionGraphClient,closeConnection)

-- ------------------------------------------------------------------------
-- Modifying the graph
-- ------------------------------------------------------------------------

-- | Create a new working version in the graph
newWorkingVersion :: VersionGraphClient -> View -> IO VersionGraphNode
newWorkingVersion versionGraphClient view =
   do
      let 
         mkVersionInfo1 versionInfo = VersionInfo1 {
            versionInfo = versionInfo,
            viewOpt = Just view
            }


         initialAct :: VersionInfo -> IO ()
         initialAct versionInfo =
            addVersion (versionDag versionGraphClient) 
               (mkVersionInfo1 versionInfo)


         updateAct :: VersionInfo -> IO ()
         updateAct versionInfo = 
            setNodeInfo 
                (versionDag versionGraphClient) 
               (mkVersionInfo1 versionInfo)
--         case parents . user $ versionInfo of
--            [parent] ->
--               doWhenVersionExists versionGraphClient parent
--                  (setNodeInfo (versionDag versionGraphClient) 
--                     (mkVersionInfo1 versionInfo))
--            _ -> error (
--               "VersionGraphClient: attempt to change parents of working "
--               ++ "version to list of length not 1")

      sinkID <- newSinkID
      parallelX <- newParallelExec

      addNewSinkWithInitial (toSimpleSource (viewInfoBroadcaster view))
         initialAct updateAct sinkID parallelX
      return (WorkingNode view)

deleteWorkingVersion :: VersionGraphClient -> View -> IO ()
deleteWorkingVersion versionGraphClient view =
   deleteVersion (versionDag versionGraphClient) (WorkingNode view)

setNewFilter0 :: VersionGraphClient -> VersionInfoFilter -> IO ()
setNewFilter0 graphClient filter =
   changeIsHidden graphClient (mkHidden filter)

mkHidden :: VersionInfoFilter -> (VersionInfo -> Bool)
mkHidden filter versionInfo = not (filterVersionInfo filter versionInfo)

changeIsHidden :: VersionGraphClient -> (VersionInfo -> Bool) -> IO ()
changeIsHidden versionGraphClient isHidden =
   VersionDag.changeIsHidden 
      (versionDag versionGraphClient) (isHidden . versionInfo)

-- ------------------------------------------------------------------------
-- Extracting the graph
-- ------------------------------------------------------------------------

-- | Get the graph corresponding to a version graph, which can then be
-- displayed.
toVersionGraphConnection :: VersionGraphClient 
   -> GraphConnection VersionInfo1 () () ()
toVersionGraphConnection versionGraphClient =
   let
      displayedGraph1 :: GraphConnection (VersionInfo1,Bool) () (Maybe Bool) ()
      displayedGraph1 = toDisplayedGraph (versionDag versionGraphClient)

      mapNode :: (VersionInfo1,Bool) -> (VersionInfo1,NodeType)
      mapNode (versionInfo1,isHidden) = 
         let
            nodeType = case (isJust (viewOpt versionInfo1),isHidden) of
               (False,False) -> checkedInType
               (False,True) -> checkedInTypeHidden
               (True,False) -> workingType
               (True,True) -> workingTypeHidden
         in
            (versionInfo1,nodeType)

      mapArc :: Maybe Bool -> ((),ArcType)
      mapArc b =
         let
            arcType = case b of
               Just True -> workingArcType
               Just False -> checkedInArcType
               Nothing -> arcTypeHidden
         in
            ((),arcType) 

      initialUpdates = [
         NewNodeType checkedInType (),
         NewNodeType workingType (),
         NewNodeType checkedInTypeHidden (),
         NewNodeType workingTypeHidden (),
         NewArcType checkedInArcType (),
         NewArcType workingArcType (),
         NewArcType arcTypeHidden ()
         ]
   in
      mapGraphConnection mapNode mapArc initialUpdates displayedGraph1  

-- | Extract GraphBack structure representing all the nodes in the
-- graph (including deleted ones).
getInputGraphBack :: VersionGraphClient 
   -> (VersionGraphNode -> VersionInfo1 -> graphBackNodeKey)
   -> IO (GraphBack VersionGraphNode graphBackNodeKey)
getInputGraphBack versionGraphClient toGraphBackNodeKey =
   VersionDag.getInputGraphBack (versionDag versionGraphClient) 
      toGraphBackNodeKey

-- ------------------------------------------------------------------------
-- Accessing VersionInfo1's
-- ------------------------------------------------------------------------

toVersionInfo :: VersionInfo1 -> VersionInfo
toVersionInfo = versionInfo

toViewOpt :: VersionInfo1 -> Maybe View
toViewOpt = viewOpt

-- ------------------------------------------------------------------------
-- Query operations
-- ------------------------------------------------------------------------

getVersionInfo :: VersionGraphClient -> ObjectVersion -> IO VersionInfo
getVersionInfo versionGraphClient version =
   do
      versionInfo1Opt <- lookupNodeKey (versionDag versionGraphClient)
         (CheckedInNode version)
      case versionInfo1Opt of
         Nothing -> error ("VersionGraphClient.getVersionInfo: "
            ++ "unknown version " ++ show version ++ ".")
         Just versionInfo1 -> return (versionInfo versionInfo1)

getVersionInfos :: VersionGraphClient -> IO [VersionInfo1]
getVersionInfos versionGraphClient 
   = getNodeInfos (versionDag versionGraphClient)
         
-- ------------------------------------------------------------------------
-- The node and arc types
-- ------------------------------------------------------------------------

checkedInType :: NodeType
checkedInType = fromString "C"

workingType :: NodeType
workingType = fromString "W"

checkedInTypeHidden :: NodeType
checkedInTypeHidden = fromString "c"

workingTypeHidden :: NodeType
workingTypeHidden = fromString "w"

checkedInArcType :: ArcType
checkedInArcType = fromString "C"

workingArcType :: ArcType
workingArcType = fromString "W"

arcTypeHidden :: ArcType
arcTypeHidden = fromString "h"

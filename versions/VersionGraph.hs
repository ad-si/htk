{- This module is responsible for the Version Graph.  Therefore it has
   two main tasks:
      (1) displaying the Version Graph;
      (2) communicating with the server.
   The functions in this file are intended to be used together with those
   of FileSys; neither supersedes the other.
   -}
module VersionGraph(
   -- version graph creation and destruction
   VersionGraphParms,
   emptyVersionGraphParms, 

   GraphParm(GraphParm),CheckedInParm(CheckedInParm),WorkingParm(WorkingParm),
   -- To configure VersionGraphParms you use the HasConfig format,
   -- with either a GraphParm (with a graph option) to configure the graph,
   -- or a CheckedInParm (with a node type option) to configure the type of
   --    a node corresponding to a checked in version,
   -- or a WorkingParm (with a node type option) to configure the type of
   --    a node corresponding to a working version.
   -- The values attached to the nodes for the last two options can be
   -- any of a (Node) or (AnyVersion) or (String) or (AnyVersion,String).
   -- An option DragAndDrop is provided for the purpose of providing drag-and-
   -- drop actions.  (This differs from NodeDragAndDrop in that it decodes the
   -- Dyn provided by the standard option NodeDragAndDrop.)

   VersionGraph, -- a graph being displayed.  Instance of Destructible
      -- A type parameterised on graphParms and nodeTypeParms
   newVersionGraph, 
      -- :: <Display sort> -> VersionGraphParms -> IO VersionGraph

   WorkingVersionId, 
      -- used for a WorkingVersion, created by 
      -- newWorkingVersion.
   AnyVersion(..),
      -- Either a Version or a WorkingVersionId.  Represents an object in the
      -- graph.

   newWorkingVersion,
      -- :: VersionGraph -> [Version] -> String -> IO WorkingVersionId
      -- Register the fact that we create a new working version which is
      -- a child of the given list of versions.  (Normally this will be 1;
      -- it should be 0 when we start a whole new project, and will be >1 
      -- when we merge two versions.)
   newVersion,
      -- :: VersionGraph -> WorkingVersionId -> Version -> (Maybe String) 
      --    -> IO ()
      -- Register that we've checked in the working version WorkingVersionId
      -- to make a new version with the given version.  Make the working
      -- version the child of (just) this version.
      -- If the String is unset we take the one from the working version;
      -- otherwise this is used to label the version.
   deleteWorkingVersion,
      -- :: VersionGraph -> WorkingVersionId -> IO ()

   -- reading the version graph
   anyVersionToNode, -- :: AnyVersion -> Node
   nodeToAnyVersion, -- :: Node -> AnyVersion

   getParents,
      -- :: VersionGraph -> AnyVersion -> IO [Version]

   -- VersionGraphParms can be configured using HasConfig framework
   -- (see file Computation).  Values of X for which HasConfig 
   -- VersionGraphParms X holds include:
   --    GlobalMenu
   --    LocalMenu (Either Version WorkingVersionId)
   ) where

import QuickReadShow
import AtomString
import Computation(done)
import Dynamics

import Concurrent
import Thread hiding (update)

import SIM(Destructible(..))
import CallServer

import Graph
import GraphDisp hiding (newArc,newGraph)
import GraphConfigure
import GraphConnection
import SimpleGraph
import qualified DisplayGraph

import FileSys
import VersionGraphService

-- ------------------------------------------------------------------------
-- Version Graph
-- ------------------------------------------------------------------------

data VersionGraph = VersionGraph {
   displayGraph :: DisplayGraph.DisplayGraph, -- the graph display
   graphContents :: VersionTypes SimpleGraph, 
      -- Our own copy of the version graph.  
      -- We operate on the version graph via this graph.
   destroyThis :: IO (),
      -- How to destroy the graph
   workingVersionIdSource :: WorkingVersionIdSource
      -- source of new working version numbers
   }

instance Destructible VersionGraph where
   destroy versionGraph = destroyThis versionGraph
   destroyed versionGraph = destroyed (graphContents versionGraph)
      -- the graphContents gets destroyed last.

-- ------------------------------------------------------------------------
-- WorkingVersionId
-- ------------------------------------------------------------------------

newtype WorkingVersionId = WorkingVersionId Int

newtype WorkingVersionIdSource = WorkingVersionIdSource (MVar Int)

newWorkingVersionIdSource :: IO WorkingVersionIdSource
newWorkingVersionIdSource = 
   do
      mVar <- newMVar 0
      return (WorkingVersionIdSource mVar)

newWorkingVersionId :: WorkingVersionIdSource -> IO WorkingVersionId
newWorkingVersionId (WorkingVersionIdSource mVar) =
   do
      number <- takeMVar mVar
      putMVar mVar (number+1)
      return (WorkingVersionId number)

instance QuickRead WorkingVersionId where
   quickRead = WrapRead (\ i -> WorkingVersionId i)

instance QuickShow WorkingVersionId where
   quickShow = WrapShow (\ (WorkingVersionId i) -> i)

-- ------------------------------------------------------------------------
-- AnyVersion
-- ------------------------------------------------------------------------

data AnyVersion = CheckedIn Version | Working WorkingVersionId

anyVersionTag = mkTyCon "VersionGraph" "AnyVersion"

instance HasTyCon AnyVersion where
   tyCon _ = anyVersionTag

-- We want to encode an AnyVersion economically (since Version's have to
-- be sent off to the server).  We try and make it so that CheckedIn
-- versions are sent especially economically; hence they are sent in 
-- unchanged unless they begin with "W" or "X" (chosen because CVS versions
-- do not begin with these letters; indeed they only begin with numbers).
encodeTwo :: Either String String -> String
encodeTwo (Right uncommon) = 'W':uncommon
encodeTwo (Left common) =
   case common of
      'W':rest -> 'X':common
      'X':rest -> 'X':common
      _ -> common

decodeTwo :: String -> Either String String
decodeTwo str =
   case str of
      'W':uncommon -> Right uncommon
      'X':common -> Left common
      common -> Left common

instance StringClass AnyVersion where
   toString (CheckedIn version) = encodeTwo (Left (toString version))
   toString (Working workingVersionId) = 
      encodeTwo (Right (show workingVersionId))
   fromString str =
      case decodeTwo str of
         Left versionStr -> CheckedIn (fromString versionStr)

anyVersionToNode :: AnyVersion -> Node
anyVersionToNode = fromString . toString

nodeToAnyVersion :: Node -> AnyVersion
nodeToAnyVersion = fromString . toString

-- ------------------------------------------------------------------------
-- Parameters for VersionGraphParms
-- The major problem is with parameters to the node types, since these
-- must also accept titles and menus which depend on strings, which
-- require lookup of values we don't pass to DisplayGraph's copy of the
-- graph.  Therefore we invent the following delayed-action configuration.
-- ------------------------------------------------------------------------

newtype NodeTypeParms nodeTypeParms => 
   DelayedParms nodeTypeParms = 
      DelayedParms (VersionGraph -> nodeTypeParms Node)

emptyDelayedParms :: NodeTypeParms nodeTypeParms => DelayedParms nodeTypeParms
emptyDelayedParms = DelayedParms (\ _ -> emptyNodeTypeParms)

evaluateDelayedParms :: NodeTypeParms nodeTypeParms 
   => VersionGraph -> DelayedParms nodeTypeParms -> nodeTypeParms Node
evaluateDelayedParms versionGraph (DelayedParms getParms) =
   getParms versionGraph

-- If (HasConfigValue option nodeTypeParms,HasMapIO option)
--    then
-- HasConfig X (DelayedParms nodeTypeParms)
-- provided that X is one of the following types:
--     option Node
--     option String
--     option AnyVersion
--     option (AnyVersion,String)
--     VersionGraph -> option Node
--     VersionGraph -> option String
--     VersionGraph -> option AnyVersion
--     VersionGraph -> option (AnyVersion,String)
-- We do this from building up from Node upwards.

instance (NodeTypeParms nodeTypeParms,HasConfigValue option nodeTypeParms)
   => HasConfig (VersionGraph -> option Node) (DelayedParms nodeTypeParms)
      where
   configUsed option _ =  
      configUsed' (bot :: option Node) (bot :: nodeTypeParms Node)
         where
            bot = bot
   ($$) getOption (DelayedParms getParms) =
      (DelayedParms (
         \ versionGraph -> (getOption versionGraph) $$$ (getParms versionGraph)
         ))

class CanGetFromNode nodeData where
   getFromNode :: VersionGraph -> Node -> IO nodeData

instance CanGetFromNode String where
   getFromNode versionGraph node =
      getNodeLabel (getGraph versionGraph) node

instance CanGetFromNode AnyVersion where
   getFromNode versionGraph node =
      return (nodeToAnyVersion node)

instance CanGetFromNode (AnyVersion,String) where
   getFromNode versionGraph node =
      do
         (anyVersion :: AnyVersion) <- getFromNode versionGraph node
         (str :: String) <- getFromNode versionGraph node
         return (anyVersion,str)

mapFromNode :: (CanGetFromNode nodeData,HasMapIO option) 
   => (VersionGraph -> option nodeData) -> (VersionGraph -> option Node)
mapFromNode getOption =
   let
      newGetOption versionGraph =
         mapIO
            (\ node -> getFromNode versionGraph node)
            (getOption versionGraph)
   in
      newGetOption

instance (NodeTypeParms nodeTypeParms,HasMapIO option,CanGetFromNode nodeData,
   HasConfig (VersionGraph -> option Node) (DelayedParms nodeTypeParms)) 
   => HasConfig (VersionGraph -> option nodeData) (DelayedParms nodeTypeParms)
      where
   configUsed option _ =
      configUsed (mapFromNode option) 
            (emptyDelayedParms :: DelayedParms nodeTypeParms)
         where
            bot = bot
   ($$) option delayedParms = (mapFromNode option) $$ delayedParms

instance (NodeTypeParms nodeTypeParms,
   HasConfig (VersionGraph -> option nodeData) (DelayedParms nodeTypeParms))
   => HasConfig (option nodeData) (DelayedParms nodeTypeParms)
      where

   configUsed option _ =
      configUsed (\ (versionGraph :: VersionGraph) -> option) 
            (emptyDelayedParms :: DelayedParms nodeTypeParms)
         where
            bot = bot
   ($$) option delayedParms = (\ (versionGraph :: VersionGraph) -> option) 
      $$ delayedParms

-- To DragAndDrop we provide a new sort of configuration.

newtype DragAndDrop nodeData = 
   DragAndDrop (nodeData -> nodeData -> IO ())
-- This is a DragAndDrop configuration for when we are sure that the
-- two nodes carry the same value, which they do for the version graph.
-- The first node was dropped onto the second node.

instance (NodeTypeParms nodeTypeParms,
   HasConfigValue NodeDragAndDrop nodeTypeParms)
   => HasConfigValue DragAndDrop nodeTypeParms
      where
   configUsed' option (_ :: nodeTypeParms nodeData) = configUsed' option
      (emptyNodeTypeParms :: nodeTypeParms nodeData)
   ($$$) (DragAndDrop node2node2act) parms =
      (NodeDragAndDrop (dragDropConvert node2node2act)) $$$
         parms

dragDropConvert :: Typeable nodeData 
   => (nodeData -> nodeData -> IO ()) -> (Dyn -> nodeData -> IO ())
dragDropConvert node2node2act =
   let
      dyn2node2act dyn node2 =
         case fromDyn dyn of 
            Just node1 -> node2node2act node1 node2
            Nothing -> error "VersionGraph: mysterious value found"
   in
      dyn2node2act

instance HasMapIO DragAndDrop where 
   mapIO a2bAct (DragAndDrop b2b2Act) =
      let
         a2a2Act aValue1 aValue2 =
            do
               bValue1 <- a2bAct aValue2
               bValue2 <- a2bAct aValue2
               b2b2Act bValue1 bValue2
      in
         DragAndDrop a2a2Act

-- ------------------------------------------------------------------------
-- VersionGraphParms
-- ------------------------------------------------------------------------


data VersionGraphParms graphParms nodeTypeParms = VersionGraphParms {
   graphParms :: graphParms,
   checkedInParms :: DelayedParms nodeTypeParms,
   workingParms :: DelayedParms nodeTypeParms
   }

emptyVersionGraphParms :: 
   (GraphParms graphParms,NodeTypeParms nodeTypeParms)
   => VersionGraphParms graphParms nodeTypeParms
emptyVersionGraphParms = VersionGraphParms {
   graphParms = emptyGraphParms,
   checkedInParms = emptyDelayedParms,
   workingParms = emptyDelayedParms
   }

newtype GraphParm option = GraphParm option
newtype CheckedInParm option = CheckedInParm option
newtype WorkingParm option = WorkingParm option

instance HasConfig option graphParms 
   => HasConfig (GraphParm option) (VersionGraphParms graphParms nodeTypeParms)
      where
   configUsed (GraphParm option) versionGraphParms =
      configUsed option (graphParms versionGraphParms)
   ($$) (GraphParm option) versionGraphParms =
      versionGraphParms {
         graphParms = option $$ (graphParms versionGraphParms)
         }

instance HasConfig option (DelayedParms nodeTypeParms) 
   => HasConfig (CheckedInParm option) 
         (VersionGraphParms graphParms nodeTypeParms)
      where
   configUsed (CheckedInParm option) versionGraphParms =
      configUsed option (checkedInParms versionGraphParms)
   ($$) (CheckedInParm option) versionGraphParms =
      versionGraphParms {
         checkedInParms = option $$ (checkedInParms versionGraphParms)
         }

instance HasConfig option (DelayedParms nodeTypeParms) 
   => HasConfig (WorkingParm option)
         (VersionGraphParms graphParms nodeTypeParms)
      where
   configUsed (WorkingParm option) versionGraphParms =
      configUsed option (workingParms versionGraphParms)
   ($$) (WorkingParm option) versionGraphParms =
      versionGraphParms {
         workingParms = option $$ (workingParms versionGraphParms)
         }

-- ------------------------------------------------------------------------
-- Constructing Version Graphs
-- ------------------------------------------------------------------------

newVersionGraph :: 
   (GraphAll dispGraph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph dispGraph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) 
   -> VersionGraphParms graphParms nodeTypeParms 
   -> IO VersionGraph
newVersionGraph displaySort versionGraphOptions =
   do
      -- (1) connect to the server
      (graphUpdate,getUpdateFromParent,closeConnection,currentStateString) 
         <- connectBroadcastOther versionGraphService
      -- (2) construct a graph connection from this.
      let
         graphConnection updateChild =
            do
               let
                  updateThread =
                     do
                        update <- getUpdateFromParent
                        updateChild update
                        updateThread
               updateThreadId <- 
                  forkIOquiet "Update Version Graph thread" updateThread
               let
                  FrozenGraph {
                     graphState' = graphState,
                     nameSourceBranch' = nameSourceBranch
                     } = read currentStateString
                  deRegister =
                     do
                        killThread updateThreadId
                        closeConnection
               return (GraphConnectionData {
                  graphState = graphState,
                  deRegister = deRegister,
                  graphUpdate = graphUpdate,
                  nameSourceBranch = nameSourceBranch
                  })
      -- (3) Construct a second graph connection which filters out
      --     nodes which don't correspond to real versions
         subGraph = SubGraph {
            nodeIn =
               (\ node -> 
                  case decodeTwo (toString node) of
                     Left realVersion -> True
                     Right workingVersion -> False
                  ),
            nodeTypeIn =
               (\ nodeTypeIn -> True)
            }

         superGraphConnection = attachSuperGraph subGraph graphConnection

      -- (4) construct the graph
      graphContents <- newGraph superGraphConnection

      workingVersionIdSource <- newWorkingVersionIdSource

      let
         mkVersionGraph displayGraph =
         -- this is used inside DisplayGraph.displayGraph to tie the knot,
         -- so it is important that it be pure (and since it will be done more
         -- than once, not excessively slow).
            let
               destroyThis =
                  do
                     destroy displayGraph
                     -- destroy this first, as it involves a connection to
                     -- graphContents
                     destroy graphContents
                     -- the deregister action will automatically close the
                     -- connection to the server.
            in
               VersionGraph {
                  displayGraph = displayGraph,
                  graphContents = graphContents,
                  destroyThis = destroyThis,
                  workingVersionIdSource = workingVersionIdSource
                  }

      -- (5) display the graph
      displayGraph <- DisplayGraph.displayGraph displaySort graphContents 
         (graphParms versionGraphOptions)
         (\ displayGraph nodeType () ->
            return (
               if nodeType == checkedInType
                  then
                     evaluateDelayedParms (mkVersionGraph displayGraph)
                        (checkedInParms versionGraphOptions)
                  else if nodeType == workingType
                     then
                        evaluateDelayedParms (mkVersionGraph displayGraph)
                           (workingParms versionGraphOptions)
                     else
                         error "Unknown node type in VersionGraph.hs"
               ) 
            )
         (\ _ arcType' () -> 
            if arcType' == arcType
               then
                  return emptyArcTypeParms
               else
                  error "Unknown arc type encountered in VersionGraph.hs"
            )
      return (mkVersionGraph displayGraph)


-- ------------------------------------------------------------------------
-- Registering New Versions
-- ------------------------------------------------------------------------

newWorkingVersion :: VersionGraph -> [Version] -> String -> IO WorkingVersionId
newWorkingVersion (VersionGraph {graphContents = graphContents,
      workingVersionIdSource = workingVersionIdSource}) parents nodeTitle =
   do
      workingVersionId <- newWorkingVersionId workingVersionIdSource
      let
         newNode = anyVersionToNode (Working workingVersionId)
         parentNodes = map (anyVersionToNode . CheckedIn) parents
      update graphContents (NewNode newNode workingType nodeTitle)
      sequence_ 
         (map
            (\ parent -> newArc graphContents arcType "" parent newNode)
            parentNodes
            )
      return workingVersionId

newVersion :: VersionGraph -> WorkingVersionId -> Version -> Maybe String 
   -> IO ()
newVersion (VersionGraph {graphContents = graphContents}) workingVersionId
      version maybeTitle =
   do
      let
         workingNode = anyVersionToNode (Working workingVersionId)
         versionNode = anyVersionToNode (CheckedIn version)
      nodeTitle <-
         case maybeTitle of
            Nothing -> getNodeLabel graphContents workingNode 
            Just title -> return title
      versionParentArcs <- getArcsIn graphContents workingNode

      update graphContents (NewNode versionNode checkedInType nodeTitle)
      sequence_
         (map
            (\ arc ->
               do
                  source <- getSource graphContents arc
                  update graphContents (DeleteArc arc)
                  newArc graphContents arcType "" source versionNode
                  done
               )
            versionParentArcs
            )
      newArc graphContents arcType "" versionNode workingNode            
      done

deleteWorkingVersion :: VersionGraph -> WorkingVersionId -> IO ()
deleteWorkingVersion (VersionGraph {graphContents = graphContents})
      workingVersionId =
   update graphContents 
      (DeleteNode (anyVersionToNode (Working workingVersionId)))

-- ------------------------------------------------------------------------
-- Reading Version Graphs
-- ------------------------------------------------------------------------

getParents :: VersionGraph -> AnyVersion -> IO [Version]
getParents (VersionGraph {graphContents = graphContents}) anyVersion =
   do
      parentArcs <- getArcsIn graphContents (anyVersionToNode anyVersion)
      parents <- mapM (getSource graphContents) parentArcs
      let
         parentVersions =
            map
               (\ node ->
                  case nodeToAnyVersion node of
                     CheckedIn version -> version
                     -- error means a working version is a parent of a version,
                     -- which shouldn't be
                  )
               parents
      return parentVersions

getGraph :: VersionGraph -> VersionTypes SimpleGraph
getGraph versionGraph = graphContents versionGraph

getVersionString :: VersionGraph -> AnyVersion -> IO String
getVersionString versionGraph anyVersion =  
   let
      node = anyVersionToNode anyVersion
      graphContents = getGraph versionGraph
   in
      getNodeLabel graphContents node  
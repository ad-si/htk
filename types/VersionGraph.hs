{- This module is responsible for the Version Graph.  Therefore it has
   two main tasks:
      (1) displaying the Version Graph;
      (2) communicating with the server.  The server uses the 
          VersionGraphService service.
   -}
module VersionGraph(
   VersionGraph, -- a graph being displayed.  Instance of Destructible
      -- A type parameterised on graphParms and nodeTypeParms
   newVersionGraph, 
      -- :: (display sort) -> Repository -> IO VersionGraph
   newVersionGraphInternal,
      -- :: (display sort) -> Repository -> VersionState -> IO VersionGraph

   versionToNode,
      -- :: ObjectVersion -> Node
      -- Converts an object version to its corresponding node.  This is only
      -- used by the Initialisation module; we don't want anyone else mucking
      -- around with the version graph!


   toVersionGraphGraph,
      -- :: VersionGraph -> VersionTypes SimpleGraph
      -- extract a version graph's actual graph.  

   toVersionGraphRepository,
      -- :: VersionGraph -> Repository
      -- extract a version graph's repository.

   selectCheckedInVersions,
      -- :: VersionGraph -> String -> IO (Maybe [ObjectVersion])
      -- Provide a list-box interface allowing the user to click on the
      -- version graph to select checked-in versions in the graph.

   commitViewInGraph, 
      -- :: VersionGraph -> View -> IO ()
      -- Commit a view in the graph (after prompting the user in the
      -- normal way) and reconnect the graph nodes.
   ) where

import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent

import Computation
import WBFiles(getServer)
import Registry
import AtomString
import UniqueString
import Sources
import Broadcaster
import Dynamics

import Spawn
import Destructible
import Events
import Channels

import BSem
import HostsPorts
import CallServer
import Lock

import SimpleForm
import MenuType
import SimpleListBox
import MarkupText
import DialogWin
import HTk hiding (Arc,Menu)

import Graph
import SimpleGraph
import DisplayGraph
import GraphDisp
import GraphConfigure
import NewNames

import VersionInfo

import VersionDB
import View
import ViewType(viewId,viewInfoBroadcaster,ViewId)
import DisplayTypes
import DisplayView
import VersionGraphClient
import Folders
import Merging
import {-# SOURCE #-} CopyVersions

-- --------------------------------------------------------------------
-- The datatypes
-- --------------------------------------------------------------------

data VersionGraph = VersionGraph {
   displayedGraph :: DisplayGraph,
   graph :: VersionTypes SimpleGraph,
   closeDownAction :: IO (),
   closedEvent :: Event (),
   repository :: Repository,
   selectCheckedInVersions :: String -> IO (Maybe [ObjectVersion]),
   commitViewInGraph1 :: View -> IO () 
      -- ^ described at the head of this module.
   }

---
-- Information we keep about working nodes.
data ViewedNode = ViewedNode {
   thisView :: View, -- view for this node 
   bSem :: BSem -- locks commit operations.
   }
   
-- --------------------------------------------------------------------
-- Opening a new VersionGraph
-- --------------------------------------------------------------------

newVersionGraph :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
   ?server :: HostPort)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> IO VersionGraph
newVersionGraph displaySort repository =
   do
      graph <- mkVersionSimpleGraph
      newVersionGraph1 displaySort repository graph (show ?server) False

newVersionGraphInternal ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> VersionState -> IO VersionGraph
newVersionGraphInternal displaySort repository versionState =
   do
      graph <- mkVersionSimpleGraphInternal versionState
      newVersionGraph1 displaySort repository graph "(Internal)" True

newVersionGraph1 :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> VersionSimpleGraph -> String -> Bool -> IO VersionGraph
newVersionGraph1 
      (displaySort 
         :: GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) repository graph title isInternal =
   do
      -- All working nodes.
      (workingNodeRegistry :: Registry Node ViewedNode) <- newRegistry

      -- graph which is connected to the server and will (via displayGraph)
      -- be displayed.  We will update the version graph by displaying
      -- this graph.
      let
         getVersionInfo1 = getVersionInfo graph

      -- This MVar will contain the actual version graph, when set up.
      versionGraphMVar <- newEmptyMVar

      -- This MVar will contain the actual displayed graph when set up.
      dispGraphMVar <- newEmptyMVar

      -- Checked-out versions are associated with two elements of the
      -- Node type.  The most important one is the current one, with
      -- which the version is currently displayed in the graph.  However
      -- there is also the original one, which is that with which the
      -- version was originally displayed.  Thus the original Node does not
      -- change. 

      let
         -- Parameters for displayGraph
         graphParms = 
            (GraphTitle title) $$
            (GlobalMenu (Menu Nothing [
               Button "Merge" doMerge,
               Button "Copy Versions" copyVersions1
               ])) $$
            (AllowClose
               (if isInternal
                  then
                     Just "Internal Version Graph cannot be closed"
                  else
                     Nothing
                  )
               ) $$
            emptyGraphParms
         
         -- getNodeTypeParms constructs the parameters for a node
         -- type
         getNodeTypeParms _ nodeType () =
            return (
               if nodeType == checkedInType
               then
                  Box $$$
                  (DoubleClickAction checkOutNode) $$$
                  (ValueTitle nodeTitle) $$$
                  (LocalMenu (Menu Nothing [
                     Button "Checkout" checkOutNode,
                     Button "View Info" viewCheckedInNode,
                     Button "Edit Info" editCheckedInNode
                     ])) $$$
                  emptyNodeTypeParms                                 
               else if nodeType == workingType
               then
                  (Color "red") $$$
                  Box $$$
                  (DoubleClickAction commitNode) $$$ 
                  (ValueTitle nodeTitle) $$$
                  (LocalMenu (Menu Nothing [
                     Button "Commit" commitNode,
                     Button "View Info" viewWorkingNode,
                     Button "Edit Info" editWorkingNode
                     ])) $$$
                  emptyNodeTypeParms
               else
                  error "VersionGraph: unrecognised NodeType" 
               )

         getArcTypeParms _ arcType () =
            return (
               if arcType == checkedInArcType
               then
                  (Color "black") $$$
                  Solid $$$
                  emptyArcTypeParms
               else if arcType == workingArcType
               then
                  (Color "red") $$$
                  Dotted $$$
                  emptyArcTypeParms
               else
                  error "VersionGraph: unrecognised ArcType"
               )

         checkOutNode :: Node -> IO ()
         checkOutNode node =
            case nodeToVersion node of
               Just version ->
                  do
                     versionInfo0 <- getVersionInfo1 node
                     if isPresent versionInfo0
                        then
                           do
                              let
                                 versionInfo1 = cleanVersionInfo versionInfo0

                              userInfo1Opt <- editVersionInfo
                                 "Checkout version" versionInfo1
                              case userInfo1Opt of
                                 Nothing -> done
                                 Just userInfo1 -> 
                                    reallyCheckOutNode userInfo1 node version
                        else
                           createErrorWin 
                              ("Version is not checked into this repository\n"
                              ++ "(It may be a parent version from another "
                              ++ "repository)") []

         reallyCheckOutNode :: UserInfo -> Node -> Version -> IO ()
         reallyCheckOutNode userInfo parentNode version =
            do
               view <- getView repository graph version
               setUserInfo view userInfo
               let 
                  versionGraphNode = WorkingNode view
                  thisNode = toNode versionGraphNode

               bSem <- newBSem
               setValue workingNodeRegistry thisNode 
                  (ViewedNode{
                     thisView = view,
                     bSem = bSem
                     })

               viewInfo <- readVersionInfo view
               update graph (NewNode thisNode workingType viewInfo)
               Graph.newArc graph workingArcType () parentNode thisNode

               versionGraph <- readMVar versionGraphMVar
               recordViewVersionGraph view versionGraph

               (Just displayedView) <- openGeneralDisplay
                  displaySort FolderDisplayType view
               addCloseDownAction displayedView (
                  update graph (DeleteNode thisNode)
                  )
               done

         commitViewInGraph1 :: View -> IO ()
         commitViewInGraph1 view =
            do
               let
                  node = toNode (WorkingNode view)
               commitNode node
                      
         commitNode :: Node -> IO ()
         commitNode node =
            do
               viewedNodeOpt <- getValueOpt workingNodeRegistry node
               viewedNodeOp viewedNodeOpt
                  (\ view ->
                     do
                        viewInfo0 <- getVersionInfo1 node
                        userInfo1Opt <- editVersionInfo
                           "Commit Version" viewInfo0
                        case userInfo1Opt of
                           Nothing -> done
                           Just userInfo1 ->
                              reallyCommitNode userInfo1 node view
                     )

         reallyCommitNode :: UserInfo -> Node -> View -> IO ()
         reallyCommitNode userInfo thisNode view =
            do
               update graph (DeleteNode thisNode)

               setUserInfo view userInfo

               newVersion <- commitView view

               -- The VersionInfoServer/VersionGraphClient will create a
               -- node corresponding to the new view.
               -- 
               -- Register an action to add arcs from it to the view node.
               let 
                  versionGraphNode = CheckedInNode newVersion
                  newNode = toNode versionGraphNode

                  delayedAct =
                     do 
                        versionInfo <- readVersionInfo view
                        update graph (NewNode thisNode workingType versionInfo)
                        Graph.newArc graph checkedInArcType () newNode thisNode
                        bSem <- newBSem
                        setValue workingNodeRegistry thisNode 
                           (ViewedNode {
                              thisView = view,
                              bSem = bSem
                              }) 

               delayedAction graph newNode delayedAct



         -- Edit the view information for a working version
         editWorkingNode :: Node -> IO ()
         editWorkingNode node =
            do
               viewedNodeOpt <- getValueOpt workingNodeRegistry node
               viewedNodeOp viewedNodeOpt
                  (\ view ->
                     do
                        viewInfo0 <- getVersionInfo1 node
                        userInfo1Opt <- editVersionInfo  "Edit Info" viewInfo0
                        case userInfo1Opt of
                           Nothing -> done
                           Just userInfo1 ->
                              do
                                 setUserInfo view userInfo1
                                 viewInfo1 <- readVersionInfo view
                                 update graph (SetNodeLabel node viewInfo1)
                     )

         -- View the view information for a working version
         viewWorkingNode :: Node -> IO ()
         viewWorkingNode node =
            do
               viewedNodeOpt <- getValueOpt workingNodeRegistry node
               viewedNodeOp viewedNodeOpt
                  (\ view ->
                     do
                        viewInfo <- getVersionInfo1 node
                        -- don't occupy the node lock while doing the display.
                        forkIO (displayVersionInfo False viewInfo)
                        done
                     )

         -- Edit the view information for a checked-in version
         editCheckedInNode :: Node -> IO ()
         editCheckedInNode node =
            do
               versionInfo0 <- getVersionInfo1 node
               userInfo1Opt <- editVersionInfo "Edit Info" 
                  versionInfo0
               case userInfo1Opt of
                  Nothing -> done
                  Just userInfo1 -> modifyUserInfo repository userInfo1

         -- Display the view information for a checked-in version
         viewCheckedInNode :: Node -> IO ()
         viewCheckedInNode node =
            do       
               versionInfo0 <- getVersionInfo1 node
               displayVersionInfo True versionInfo0

         -- Extract the title for a node
         nodeTitle :: Node -> IO String
         nodeTitle node =
            do
               versionInfo0 <- getVersionInfo1 node
               let
                  user0 = user versionInfo0
                  label0 = label user0
               
                  identifier1 =
                     if label0 == ""
                        then 
                           show (version user0)
                        else
                           label0

                  identifier2 = 
                     if isPresent versionInfo0
                        then
                           identifier1
                        else
                           "(" ++ identifier1 ++ ")"
               return identifier2

         -- Function to be used to select checked-in versions.
         -- The first String is used as the window title.
         selectCheckedInVersions :: String -> IO (Maybe [ObjectVersion])
         selectCheckedInVersions title =
            do
               -- retrieve the graph.
               dispGraph <- readMVar dispGraphMVar

               -- The difficult part is indicating what is to be
               -- merged.  We do this using a ListBox.
               topLevel <- createToplevel [
                  text title
                  ]

               messageWindow <- newLabel topLevel
                  [text (
                     "Doubleclick versions in graph to add to list;\n"++
                     "Click versions in list to remove them.")]

               listBox <- newSimpleListBox topLevel mergeLabel []

               confirmFrame <- newFrame topLevel []

               okButton <- newButton confirmFrame [text "OK"]
               cancelButton <- newButton confirmFrame [text "Cancel"]

               okClicked <- clicked okButton
               cancelClicked <- clicked cancelButton

               pack okButton [Side AtLeft]
               pack cancelButton [Side AtRight]
               pack messageWindow [Side AtTop]
               pack listBox [Side AtTop]
               pack confirmFrame [Side AtTop]

               -- Construct event for delete actions in this list.
               (deleteEvent :: Event [SimpleListBoxItem MergeCandidate],
                  terminator) <- bindSelection listBox

               let
                  mkMergeCandidate :: WrappedNode node 
                     -> IO (Maybe MergeCandidate)
                  mkMergeCandidate (WrappedNode node0) =
                     do
                        mergeNode0 <- getNodeValue dispGraph node0
                        let
                           (mergeNode :: Node) 
                              = dynCast "VersionGraph.mkMergeCandidate"
                                 mergeNode0

                        case nodeToVersion mergeNode of
                           Nothing -> -- not a version, must be a view.
                              do
                                 createErrorWin 
                                    "You may only select checked-in versions"
                                    []
                                 return Nothing
                           Just version -> 
                              do
                                 (mergeLabel :: String) <- nodeTitle mergeNode

                                 let
                                    mergeCandidate = MergeCandidate {
                                       mergeNode = mergeNode,
                                       mergeLabel = mergeLabel,
                                       mergeVersion = version
                                       }
                                 return (Just mergeCandidate)

                  -- This function is passed to getMultipleNodes for the
                  -- graph and returns the nodes to merge, or Nothing
                  -- if the operation is to be cancelled.
                  getNodes :: Event (WrappedNode node) 
                     -> IO (Maybe [MergeCandidate])
                  getNodes newVersion =
                     do
                         let
                            -- Now here is the event for the business of
                            -- getNodes 
                            getNodesEvent :: Event (Maybe [MergeCandidate])
                            getNodesEvent =
                                  (do
                                     okClicked
                                     always (
                                        do
                                           mergeCandidates <- getItems
                                              listBox
                                           return (Just mergeCandidates)
                                        )
                                  )
                               +> (do
                                     cancelClicked
                                     return Nothing
                                  )
                               +> (do
                                     (selection :: WrappedNode node)
                                        <- newVersion
                                     always (
                                        do
                                           mergeCandidateOpt <- 
                                              mkMergeCandidate selection
                                           case mergeCandidateOpt of
                                              Just mergeCandidate ->
                                                 do
                                                    addItemAtEnd listBox 
                                                       mergeCandidate
                                                    done
                                              Nothing -> done
                                        )
                                     getNodesEvent
                                 )
                               +> (do
                                     itemsToDelete <- deleteEvent
                                     always (mapM_
                                        (\ listBoxItem ->
                                           deleteItem listBox listBoxItem
                                           )
                                        itemsToDelete
                                        )
                                     getNodesEvent
                                 )

                         sync getNodesEvent

               -- Do the business.
               nodesToMergeOpt <- getMultipleNodes dispGraph getNodes

               -- Clean up
               terminator
               destroy topLevel

               -- Get object versions out
               let
                  objectVersionsOpt = case nodesToMergeOpt of
                     Nothing -> Nothing
                     Just nodes -> Just (map mergeVersion nodes)
               return objectVersionsOpt

         -- Function to do when the user asks to copy versions.
         copyVersions1 :: IO ()
         copyVersions1 =
            do
               versionGraph <- readMVar versionGraphMVar
               copyVersions versionGraph
                 
         -- Function to be executed when the user requests a merge.
         -- We can only merge checked-in versions.
         doMerge :: IO ()
         doMerge =
            do
               objectVersionsOpt <- selectCheckedInVersions "Versions to Merge"
               case objectVersionsOpt of
                  Nothing -> done
                  Just [] -> createErrorWin "No versions specified!" []
                  Just [_] -> createErrorWin "Only one version specified" []
                  Just objectVersions ->
                     do
                        -- Go ahead.
                        viewWE <- mergeNodes repository graph (
                           map Right objectVersions)
                        case fromWithError viewWE of
                           Left mess -> createErrorWin mess []
                           Right view ->
                              do
                                 -- Create a node corresponding to view,
                                 -- and to its parent nodes.
                                 let
                                    versionGraphNode = WorkingNode view
                                    thisNode = toNode versionGraphNode

                                 (parents0 :: [ObjectVersion]) 
                                    <- parentVersions view 
                                 let
                                    (parents1 :: [Node]) = map 
                                       (toNode . CheckedInNode) parents0

                                 bSem <- newBSem 
                                 setValue workingNodeRegistry thisNode
                                    (ViewedNode{
                                       thisView = view,
                                       bSem = bSem
                                       })

                                 viewInfo0 <- readVersionInfo view
                                 userInfo1Opt <- editVersionInfo
                                    "Alter Merge Info" viewInfo0

                                 viewInfo1 <- case userInfo1Opt of
                                    Nothing -> return viewInfo0
                                    Just userInfo1 ->
                                       do
                                          setUserInfo view userInfo1
                                          readVersionInfo view

                                 synchronize bSem (
                                    do
                                       update graph (NewNode thisNode 
                                          workingType viewInfo1)
                                       mapM_
                                          (\ parent -> 
                                             Graph.newArc graph 
                                                workingArcType
                                                () parent thisNode
                                             )
                                          parents1
                                    )

                                 (Just displayedView) <- openGeneralDisplay
                                    displaySort FolderDisplayType view
                                 addCloseDownAction displayedView (
                                    update graph (DeleteNode thisNode)
                                    )
                                 done
                                 

      -- Construct the graph
      (displayedGraph,dispGraph) <- displayGraph0 displaySort graph graphParms
         getNodeTypeParms getArcTypeParms

      putMVar dispGraphMVar dispGraph

      -- Close-down stuff
      destroyedChannel <- newChannel
      let
         closeDownAction' =
            do
               destroy displayedGraph

         destructorThread =
            do
               sync (destroyed displayedGraph)
               sendIO destroyedChannel ()


      closeDownAction <- doOnce closeDownAction'

      spawn destructorThread

      let
         versionGraph = VersionGraph {
            displayedGraph = displayedGraph,
            graph = graph,
            closeDownAction = closeDownAction,
            closedEvent = receive destroyedChannel,
            repository = repository,
            selectCheckedInVersions = selectCheckedInVersions,
            commitViewInGraph1 = commitViewInGraph1
            }

      putMVar versionGraphMVar versionGraph
      -- And return it
      return versionGraph


-- Operation on a viewed node, protected so that it is a no-op
-- if tried when such an operation is already going on.  This
-- protects us, hopefully, against double-clicks on nodes.
viewedNodeOp :: Maybe ViewedNode -> (View -> IO ()) -> IO ()
viewedNodeOp Nothing _ = done
viewedNodeOp (Just viewedNode) act =
   do
      let
         bs = bSem viewedNode

      proceed <- tryAcquire bs
      if proceed
         then
            do
               act (thisView viewedNode)
               release bs
         else
            done


-- Type used exclusively by the doMerge function to indicate a node being
-- merged
data MergeCandidate = MergeCandidate {
   mergeNode :: Node,
   mergeLabel :: String,
   mergeVersion :: ObjectVersion
   }

-- --------------------------------------------------------------------
-- Instances of Destroyable/Destructible
-- --------------------------------------------------------------------

instance Destroyable VersionGraph where
   destroy versionGraph = closeDownAction versionGraph

instance Destructible VersionGraph where
   destroyed versionGraph = closedEvent versionGraph
         
-- --------------------------------------------------------------------
-- Getting various things out of a VersionGraph.
-- --------------------------------------------------------------------

toVersionGraphGraph :: VersionGraph -> VersionTypes SimpleGraph
toVersionGraphGraph = graph

toVersionGraphRepository :: VersionGraph -> Repository
toVersionGraphRepository = repository

-- --------------------------------------------------------------------
-- Get version parameters 
-- --------------------------------------------------------------------

---
-- Calls getVersionPars for checking out a version
getVersionParsCheckOut :: String -> String -> IO (Maybe String)
getVersionParsCheckOut = getVersionPars "Checking Out"

---
-- calls getVersionPars for committing a version
getVersionParsCommit :: String -> String -> IO (Maybe String)
getVersionParsCommit = getVersionPars "Committing"

---
-- Get the parameters for a new version, which at present just consist
-- of the String to display as the node title.
-- If this returns Nothing, it means the user cancelled the operation.
-- We use the title of the parent version as the default
-- @param operation -- "Checking out" or "Committing", describing the 
--    operation.
-- @param parentUserString -- user-defined String attached to the parent node
-- @param parentString -- result of nodeTitle for the parent node.  This will
--    be the parentUserString if that isn't "", otherwise the version string
--    for the node. 
getVersionPars :: String -> String -> String -> IO (Maybe String)
getVersionPars operation parentUserString parentString =
   do
      let
         form = newFormEntry "Version Title" parentUserString
         windowTitle = operation++" "++parentString
      doForm windowTitle form

-- --------------------------------------------------------------------
-- commitViewInGraph. 
-- --------------------------------------------------------------------

commitViewInGraph :: View -> IO ()
commitViewInGraph view =
   do
      versionGraphOpt <- getViewVersionGraph view
      case versionGraphOpt of
         Nothing -> done
         Just versionGraph -> commitViewInGraph1 versionGraph view

-- --------------------------------------------------------------------
-- Another global registry (sigh).  Maps views to the corresponding
-- VersionGraph.
-- --------------------------------------------------------------------

recordViewVersionGraph :: View -> VersionGraph -> IO ()
recordViewVersionGraph view versionGraph =
   setValue viewToVersionGraphRegistry (viewId view) versionGraph

getViewVersionGraph :: View -> IO (Maybe VersionGraph) 
getViewVersionGraph view 
   = getValueOpt viewToVersionGraphRegistry (viewId view) 

viewToVersionGraphRegistry :: Registry ViewId VersionGraph
viewToVersionGraphRegistry = unsafePerformIO newRegistry
{-# NOINLINE viewToVersionGraphRegistry #-}


    
   
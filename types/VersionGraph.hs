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
      -- :: Repository -> IO VersionGraph
      -- The server is taken from the WBFiles --uni-server parameter.

   versionToNode,
      -- :: ObjectVersion -> Node
      -- Converts an object version to its corresponding node.  This is only
      -- used by the Initialisation module; we don't want anyone else mucking
      -- around with the version graph!
   ) where

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

import CallServer

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

import VersionDB
import View
import ViewType(viewId,titleSource)
import VersionGraphService
import DisplayTypes
import DisplayView
import VersionGraphClient
import Folders
import Merging

-- --------------------------------------------------------------------
-- The datatypes
-- --------------------------------------------------------------------

data VersionGraph = VersionGraph {
   displayedGraph :: DisplayGraph,
   graph :: VersionTypes SimpleGraph,
   closeDownAction :: IO (),
   closedEvent :: Event ()
   }

---
-- Information we keep about working nodes.
data ViewedNode = ViewedNode {
   thisView :: View, -- view for this node
   parent :: [Node] -- (checked-in) parent version.
   }
   
-- --------------------------------------------------------------------
-- Opening a new VersionGraph
-- --------------------------------------------------------------------

---
-- The server is taken from the WBFiles --uni-server parameter.
--
-- We use fixIO
newVersionGraph :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> IO VersionGraph
newVersionGraph 
      (displaySort 
         :: GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) repository =
   do
      -- All working nodes.
      (workingNodeRegistry :: Registry Node ViewedNode) <- newRegistry

      -- Source of unique strings for working-version arcs.
      arcStringSource <- newUniqueStringSource

      -- graph which is connected to the server and will (via displayGraph)
      -- be displayed.  We will update the version graph by displaying
      -- this graph.
      let
         graph = versionGraph

      -- This MVar will contain the actual displayed graph, when set up.
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
            (GraphTitle "Versions") $$
            (GlobalMenu (Menu Nothing [Button "Merge" doMerge])) $$
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
                     Button "Checkout" checkOutNode])) $$$
                  emptyNodeTypeParms                                 
               else if nodeType == workingType
               then
                  (Color "red") $$$
                  Circle $$$
                  (DoubleClickAction commitNode) $$$ 
                  (ValueTitle nodeTitle) $$$
                  (LocalMenu (Menu Nothing [
                     Button "Commit" commitNode,
                     Button "Rename" renameNode])) $$$
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

         nodeTitle :: Node -> IO String
         nodeTitle node =
            do
               nodeLabel1 <- getNodeLabel graph node
               -- We accept nodeLabel1 unless it is empty.
               -- If it's empty, we display the version string,
               -- or that of the parent in brackets.
               case (nodeLabel1,nodeToVersion node) of
                  (_:_,_) -> return nodeLabel1
                  ("",Just version) -> return (toString version)
                  ("",Nothing) ->
                     do
                        (Just viewedNode) 
                           <- getValueOpt workingNodeRegistry node
                        case parent viewedNode of
                           parent1 : _ -> case nodeToVersion parent1 of
                              Just version -> 
                                 return ("["++toString version++"]")

         checkOutNode :: Node -> IO ()
         checkOutNode node =
            case nodeToVersion node of
               Just version ->
                  do
                     parentString <- nodeTitle node
                     parentUserString <- getNodeLabel graph node
                     titleOpt <- getVersionParsCheckOut 
                        parentUserString parentString
                     case titleOpt of
                        Nothing -> done
                        Just title ->
                           reallyCheckOutNode title node version

         reallyCheckOutNode :: String -> Node -> Version -> IO ()
         reallyCheckOutNode title parentNode version =
            do
               view <- getView repository version
               broadcast (titleSource view) title
               let versionGraphNode = WorkingNode view
               let thisNode = toNode versionGraphNode
               thisArc <- newWorkingArc arcStringSource
               setValue workingNodeRegistry thisNode 
                  (ViewedNode{
                     thisView = view,
                     parent = [parentNode]
                     })
               update graph (NewNode thisNode workingType title)
               update graph (NewArc thisArc workingArcType () 
                  parentNode thisNode) 
               (Just displayedView) <- openGeneralDisplay
                  displaySort FolderDisplayType view
               addCloseDownAction displayedView (
                  update graph (DeleteNode thisNode)
                  )
               done

         commitNode :: Node -> IO ()
         commitNode node =
            do
               viewedNodeOpt <- getValueOpt workingNodeRegistry node
               case viewedNodeOpt of
                  Nothing -> done
                  -- this happens if you try to commit the same node twice
                  -- simultaneously
                  Just viewedNode -> 
                     do
                        parentString <- nodeTitle node
                        parentUserString <- getNodeLabel graph node
                        titleOpt <- getVersionParsCommit 
                           parentUserString parentString
                        case titleOpt of
                           Nothing -> done
                           Just title ->
                              reallyCommitNode title node (thisView viewedNode)

         reallyCommitNode :: String -> Node -> View -> IO ()
         reallyCommitNode title thisNode view =
            do
               (nodeDataOpt :: Maybe ([Node],SimpleBroadcaster String)) <-
                  transformValue workingNodeRegistry thisNode
                  (\ viewedNodeOpt ->
                     let
                        getNodeData viewedNode = (parent viewedNode,
                           titleSource (thisView viewedNode))
                     in
                        return (Nothing,fmap getNodeData viewedNodeOpt)
                     )
               -- This atomically empties the working node data for 
               -- thisNode,
               -- returning Nothing if for some reason someone else
               -- has already managed to commit this view.
               -- This makes it slightly safer to reconnect
               -- thisNode, as we are about to do . . .
               case nodeDataOpt of
                  Nothing -> done
                  Just (parentNodes,titleSource) ->
                     do
                        update graph (DeleteNode thisNode)
                        newVersion <- commitView view
                        -- Create a new checked-in node for this
                        -- version
                        let 
                           versionGraphNode 
                              = CheckedInNode newVersion
                           newNode = toNode versionGraphNode
                        update graph (NewNode newNode checkedInType title)
                        mapM_
                           (\ parentNode ->
                              do
                                 newArc <- newCheckedInArc graph
                                 update graph (NewArc newArc checkedInArcType
                                    () parentNode newNode)
                              )
                           parentNodes
                  
                        -- Add a new node for the view, and add it
                        -- to this new node.
                        newArc2 <- newWorkingArc arcStringSource

                        setValue workingNodeRegistry thisNode 
                           (ViewedNode {
                              thisView = view,
                              parent = [newNode]
                              }) 

                        update graph (NewNode thisNode workingType title)
                        update graph (NewArc newArc2 workingArcType ()
                           newNode thisNode)

                        broadcast titleSource title

         -- rename a working version
         renameNode :: Node -> IO ()
         renameNode node =
            do
               viewedNodeOpt <- getValueOpt workingNodeRegistry node
               case viewedNodeOpt of
                  Nothing -> done -- could happen with committed nodes.
                  Just viewedNode ->
                     do
                        let
                           titleSource0 = titleSource (thisView viewedNode)
                        title0 <- readContents titleSource0
                        let
                           titleForm = newFormEntry "New Title" title0
                        title1Opt <- doForm "Change View Title" titleForm
                        case title1Opt of
                           Nothing -> done
                           Just title1 -> 
                              do
                                 broadcast titleSource0 title1
                                 update graph (SetNodeLabel node title1)

         -- Function to be executed when the user requests a merge.
         doMerge :: IO ()
         doMerge =
            do
               -- retrieve the graph.
               dispGraph <- readMVar dispGraphMVar

               -- The difficult part is indicating what is to be
               -- merged.  We do this using a ListBox.
               topLevel <- createToplevel [
                  text "Versions to Merge"
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
                  mkMergeCandidate :: WrappedNode node -> IO MergeCandidate
                  mkMergeCandidate (WrappedNode node0) =
                     do
                        mergeNode0 <- getNodeValue dispGraph node0
                        let
                           (mergeNode :: Node) 
                              = dynCast "VersionGraph.mkMergeCandidate"
                                 mergeNode0

                        (mergeLabel :: String) <- nodeTitle mergeNode

                        contents <- case nodeToVersion mergeNode of
                           Nothing -> -- not a version, must be a view.
                              do
                                 workingNodeOpt <- getValueOpt
                                    workingNodeRegistry mergeNode
                                 case workingNodeOpt of
                                    Just viewNode 
                                       -> return (Left (thisView viewNode))
                           Just version -> return (Right version)

                        let 
                           mergeCandidate = MergeCandidate {
                              mergeNode = mergeNode,
                              mergeLabel = mergeLabel,
                              contents = contents
                              }

                        return mergeCandidate

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
                                           mergeCandidate <- 
                                              mkMergeCandidate selection
                                           addItemAtEnd listBox 
                                              mergeCandidate
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

               case nodesToMergeOpt of
                  Nothing -> done
                  Just [] -> createErrorWin "No versions specified!" []
                  Just [_] -> createErrorWin "Only one version specified" []
                  Just mergeCandidates ->
                     do
                        -- Go ahead.
                        viewWE <- mergeNodes repository (
                           map contents mergeCandidates)
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

                                 setValue workingNodeRegistry thisNode
                                    (ViewedNode{
                                       thisView = view,
                                       parent = parents1
                                       })

                                 title <- readContents (titleSource view)

                                 update graph (NewNode thisNode workingType 
                                    title)
                                 mapM_
                                    (\ parent -> 
                                       do
                                          thisArc 
                                             <- newWorkingArc arcStringSource
                                          update graph (NewArc thisArc
                                             workingArcType () parent thisNode)
                                       ) 
                                    parents1

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
               

      spawn destructorThread

      closeDownAction <- doOnce closeDownAction'
               
      -- And return it
      return (VersionGraph {
         displayedGraph = displayedGraph,
         graph = graph,
         closeDownAction = closeDownAction,
         closedEvent = receive destroyedChannel
         }) 

-- Type used exclusively by the doMerge function to indicate a node being
-- merged
data MergeCandidate = MergeCandidate {
   mergeNode :: Node,
   mergeLabel :: String,
   contents :: Either View ObjectVersion
   }

-- --------------------------------------------------------------------
-- Instances of Destroyable/Destructible
-- --------------------------------------------------------------------

instance Destroyable VersionGraph where
   destroy versionGraph = closeDownAction versionGraph

instance Destructible VersionGraph where
   destroyed versionGraph = closedEvent versionGraph
         
-- --------------------------------------------------------------------
-- Talking to the server
-- --------------------------------------------------------------------
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



    
   
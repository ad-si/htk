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

import Computation
import WBFiles(getServer)
import Registry
import AtomString
import UniqueString
import Sources
import Broadcaster

import Spawn
import Destructible
import Events
import Channels

import CallServer

import SimpleForm

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
import Folders

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
-- A VersionGraphNode represents either a checked-in version or a view.
-- Only the checked-in nodes are sent to the server.
data VersionGraphNode =
      CheckedInNode Version
   |  WorkingNode View

---
-- Information we keep about working nodes.
data ViewedNode = ViewedNode {
   thisView :: View, -- view for this node
   parent :: Node -- (checked-in) parent version.
   }
   
-- --------------------------------------------------------------------
-- The relationship between Node/Arc values and the corresponding
-- versions/values
-- --------------------------------------------------------------------

---
-- Each Node contains a String.  This Node has the following format:
-- "C[version]" for checked-in versions
-- "W[viewid]" for views.
-- Map a VersionGraphNode to the corresponding node
toNode :: VersionGraphNode -> Node
toNode (CheckedInNode version) = fromString . ('C' :) . toString $ version
toNode (WorkingNode view) = fromString . ('W' :) . show . viewId $ view

---
-- Check if node is checked in
nodeIsCheckedIn :: Node -> Bool
nodeIsCheckedIn node = 
   case toString node of
      'C' : versionString -> True
      _ -> False


---
-- Returns the node associated with a version
versionToNode :: ObjectVersion -> Node
versionToNode version = toNode (CheckedInNode version)

---
-- Returns the version associated with a node, if it is checked in.
nodeToVersion :: Node -> Maybe Version
nodeToVersion node =
   case toString node of
      'C' : versionString -> Just (fromString versionString)
      _ -> Nothing

---
-- The format of Arcs is as follows.  Checked-in arcs are generated with
-- names from getNewName on the name source branch, so their names begin
-- with ".".  For other arcs, these Strings are generated by 
-- UniqueString.newUniqueString (and so cannot begin with a period).
-- 
-- Generate a new checked-in arc.
newCheckedInArc :: VersionTypes SimpleGraph -> IO Arc
newCheckedInArc simpleGraph =
   do
      let nameSource = getNameSource simpleGraph
      str <- getNewName nameSource
      return (fromString str)

---
-- newWorkingArc generates a new working arc.  For this we
-- need a UniqueStringSource
newWorkingArc :: UniqueStringSource -> IO Arc
newWorkingArc uniqueStringSource =
   do
      str <- newUniqueString uniqueStringSource
      return (fromString str)

---
-- Check if Arc is checked in
arcIsCheckedIn :: Arc -> Bool
arcIsCheckedIn arc =
   case toString arc of
      '.':_ -> True
      _ -> False      

---
-- Filter an Update to discover if it should be sent to the server.
-- A large number of operations are forbidden.
filterUpdate :: VersionTypes Update -> Bool
filterUpdate (NewNode node nodeType nodeLabel) = nodeIsCheckedIn node
filterUpdate (DeleteNode node) = nodeIsCheckedIn node
filterUpdate (NewArc arc arcType arcLabel nodeFrom nodeTo) =
   arcIsCheckedIn arc
filterUpdate (DeleteArc arc) = arcIsCheckedIn arc
filterUpdate update = 
   error ("VersionGraph error: update "++show update++" not handled")

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
      (graph :: VersionTypes SimpleGraph,closeConnection :: IO ()) 
         <- connectToServer

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
                  emptyNodeTypeParms                                 
               else if nodeType == workingType
               then
                  (Color "red") $$$
                  Circle $$$
                  (DoubleClickAction commitNode) $$$ 
                  (ValueTitle nodeTitle) $$$
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
                        case nodeToVersion (parent viewedNode) of
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
                     parent = parentNode
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
               (nodeDataOpt :: Maybe (Node,SimpleBroadcaster String)) <-
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
                  Just (parentNode,titleSource) ->
                     do
                        update graph (DeleteNode thisNode)
                        newVersion <- commitView view
                        -- Create a new checked-in node for this
                        -- version
                        let 
                           versionGraphNode 
                              = CheckedInNode newVersion
                           newNode = toNode versionGraphNode
                        newArc1 <- newCheckedInArc graph
                        update graph (NewNode newNode checkedInType title)
                        update graph (NewArc newArc1 
                           checkedInArcType () parentNode newNode)
                  
                        -- Add a new node for the view, and add it
                        -- to this new node.
                        newArc2 <- newWorkingArc arcStringSource

                        setValue workingNodeRegistry thisNode 
                           (ViewedNode {
                              thisView = view,
                              parent = newNode
                              }) 

                        update graph (NewNode thisNode workingType title)
                        update graph (NewArc newArc2 workingArcType ()
                           newNode thisNode)

                        broadcast titleSource title

      -- Construct the graph
      displayedGraph <- displayGraph displaySort graph graphParms
         getNodeTypeParms getArcTypeParms

      -- Close-down stuff
      destroyedChannel <- newChannel
      let
         closeDownAction' =
            do
               destroy displayedGraph

         destructorThread =
            do
               sync (destroyed displayedGraph)
               destroy graph
               closeConnection 
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

---
-- connectToServer generates a new graph connected to the version
-- graph in the server.  However only nodes corresponding to 
-- checked-in versions get passed on.
-- The returned action closes the server connection.
connectToServer :: IO (VersionTypes SimpleGraph,IO ())
connectToServer =
   do
      (updateServer,getNextUpdate,closeConnection,initialiser) 
          <- connectBroadcastOther versionGraphService
      let
         FrozenGraph {
            graphState' = cannedGraph,
            nameSourceBranch' = nameSourceBranch
            } = read initialiser

         graphConnection updateSink =
            do
               let
                  updateThread =
                     do
                        nextUpdate <- getNextUpdate
                        updateSink nextUpdate
                        updateThread
                   
                  graphUpdate update = 
                     if filterUpdate update 
                        then
                           updateServer update
                        else
                           done

               killUpdateThread <- spawn updateThread

               return (GraphConnectionData {
                  graphState = cannedGraph,
                  deRegister = (
                     do
                        killUpdateThread
                        closeConnection
                     ),
                  graphUpdate = graphUpdate,
                  nameSourceBranch = nameSourceBranch 
                  })

      graph <- Graph.newGraph graphConnection  
      return (graph,closeConnection)
      
 
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



    
   
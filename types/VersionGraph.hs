-- |
-- Description: Display Version Graph
-- 
-- This module is responsible for the Version Graph.  Therefore it has
-- two main tasks:
--    (1) displaying the Version Graph;
--    (2) communicating with the server.  The server uses the 
--        VersionGraphService service.
module VersionGraph(
   VersionGraph, -- a graph being displayed.  Instance of Destructible
      -- A type parameterised on graphParms and nodeTypeParms
   newVersionGraph, 
      -- :: (display sort) -> Repository -> IO VersionGraph
   newVersionGraphInternal,
      -- :: (display sort) -> Repository -> VersionState -> IO VersionGraph

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

   toVersionGraphClient,
      -- :: VersionGraph -> VersionGraphClient
   ) where

import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception

import Computation
import Sources
import Dynamics
import ExtendedPrelude(mapEq,mapOrd)
import Messages
import Thread(forkIODebug)

import Spawn
import Destructible
import Events
import Channels

import BSem
import HostsPorts hiding (user)
import Lock

import MenuType
import SimpleListBox
import HTk hiding (Arc,Menu)

import Graph
import DisplayGraph
import GraphDisp
import GraphConfigure

import VersionInfo
import VersionState
import VersionInfoFilter

import VersionDB
import View
import DisplayView
import VersionGraphClient
import Folders
import Merging
import ToggleAdminStatus
import ManagePermissions
import {-# SOURCE #-} CopyVersions

-- --------------------------------------------------------------------
-- The datatypes
-- --------------------------------------------------------------------

data VersionGraph = VersionGraph {
   displayedGraph :: DisplayGraph,
   graph :: VersionGraphClient,
   closeDownAction :: IO (),
   closedEvent :: Event (),
   repository :: Repository,
   selectCheckedInVersions :: String -> IO (Maybe [ObjectVersion])
   }
   
-- --------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------

instance Eq VersionGraph where
   (==) = mapEq repository

instance Ord VersionGraph where
   compare = mapOrd repository

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
      graph <- mkVersionGraphClient
      newVersionGraph1 displaySort repository graph (show ?server) False

newVersionGraphInternal ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> VersionState -> IO VersionGraph
newVersionGraphInternal displaySort repository versionState =
   do
      graph <- mkVersionGraphClientInternal versionState
      newVersionGraph1 displaySort repository graph "(Local)" True

newVersionGraph1 :: 
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Repository -> VersionGraphClient -> String -> Bool -> IO VersionGraph
newVersionGraph1 
      (displaySort 
         :: GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms) repository graphClient title isInternal =
   do
      -- graph which is connected to the server and will (via displayGraph)
      -- be displayed.  We will update the version graph by displaying
      -- this graph.
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

      adminStatus <- newAdminStatus repository

      let
         -- Parameters for displayGraph
         graphParms = 
            (GraphTitle title) $$
            (GlobalMenu (Menu Nothing [
               Button "Merge" doMerge,
               Button "Copy Versions To ..." copyVersions1,
               Button "Select Versions to View" 
                  (setNewFilter graphClient),
               Button "View or Edit Global Permissions"
                  (editGlobalPermissions repository),
               Button "Toggle Admin Status" 
                  (toggleAdminStatus adminStatus)
               ])) $$
            (ActionWrapper (\ act ->
               do
                  forkIODebug (displayErrors act)
                  done
               )) $$
            (if isInternal
               then
                  AllowClose (
                     do
                        errorMess "Internal Version Graph cannot be closed"
                        return False
                     )
               else
                  defaultAllowClose
               ) $$
            emptyGraphParms
         
         -- getNodeTypeParms constructs the parameters for a node
         -- type
         getNodeTypeParms :: DisplayGraph -> NodeType -> ()
            -> IO (nodeTypeParms (Node,VersionInfo1))
         getNodeTypeParms _ nodeType () =
            let
               parms1 = getNodeTypeParms1 nodeType
               parms2 = coMapNodeTypeParms snd parms1
            in
               return parms2
   
         getNodeTypeParms1 :: NodeType -> nodeTypeParms VersionInfo1
         getNodeTypeParms1 nodeType =            
            if nodeType == checkedInType
            then
               Box $$$
               (DoubleClickAction checkOutNode) $$$
               (ValueTitle nodeTitle) $$$
               (LocalMenu (Menu Nothing [
                  Button "Checkout" checkOutNode,
                  Button "View Info" viewCheckedInNode,
                  Button "Edit Info" editCheckedInNode,
                  Button "Delete" (mDelCheckedInNode upDeleteCount),
                  Button "Undelete" (mDelCheckedInNode downDeleteCount)
                  ])) $$$
               emptyNodeTypeParms                                 
            else if nodeType == workingType
            then
               (Color "red") $$$
               Box $$$
               (DoubleClickAction commitVersionInfo1) $$$ 
               (ValueTitle nodeTitle) $$$
               (LocalMenu (Menu Nothing [
                  Button "Commit" commitVersionInfo1,
                  Button "View Info" viewWorkingNode,
                  Button "Edit Info" editWorkingNode,
                  Button "Delete" (mDelCheckedInNode upDeleteCount),
                  Button "Undelete" (mDelCheckedInNode downDeleteCount)
                  ])) $$$
               emptyNodeTypeParms
            else if nodeType == checkedInTypeHidden
            then
               Box $$$
               (LocalMenu (Menu Nothing [
                  Button "Checkout" checkOutNode,
                  Button "View Info" viewCheckedInNode,
                  Button "Edit Info" editCheckedInNode,
                  Button "Delete" (mDelCheckedInNode upDeleteCount),
                  Button "Undelete" (mDelCheckedInNode downDeleteCount)
                  ])) $$$
               (ValueTitle nodeTitle) $$$
               (staticFontStyle BoldItalicFontStyle) $$$
               emptyNodeTypeParms 
            else if nodeType == workingTypeHidden
            then
               (Color "red") $$$
               Box $$$
               (ValueTitle nodeTitle) $$$
               (LocalMenu (Menu Nothing [
                  ])) $$$ -- TBD
               (staticFontStyle BoldItalicFontStyle) $$$
               emptyNodeTypeParms 
            else
               error "VersionGraph: unrecognised NodeType"                

         staticFontStyle :: FontStyle -> FontStyleSource value
         staticFontStyle fontStyle = FontStyleSource
            (\ _ -> return (staticSimpleSource fontStyle))

         getArcTypeParms :: DisplayGraph -> ArcType -> ()
            -> IO (arcTypeParms (Arc,()))
         getArcTypeParms _ arcType () =
            let
               parms1 = getArcTypeParms1 arcType
               parms2 = coMapArcTypeParms snd parms1
            in
               return parms2

         getArcTypeParms1 :: ArcType -> arcTypeParms ()
         getArcTypeParms1 arcType =
            if arcType == checkedInArcType
            then
               (Color "black") $$$
               Solid $$$
               emptyArcTypeParms
            else if arcType == workingArcType
            then
               (Color "red") $$$
               Solid $$$
               emptyArcTypeParms               
            else if arcType == arcTypeHidden
            then
               Dotted $$$
               emptyArcTypeParms
            else
               error "VersionGraph: unrecognised ArcType"

         checkOutNode :: VersionInfo1 -> IO ()
         checkOutNode versionInfo1 =
            let
               versionInfo0 = toVersionInfo versionInfo1
            in
               doOp (
                  if isPresent versionInfo0
                     then
                        do
                           let
                              version0 = version . user $ versionInfo0 
                              versionInfo1 = cleanVersionInfo versionInfo0

                           userInfo1Opt <- editVersionInfo
                              "Checkout version" versionInfo1
                           case userInfo1Opt of
                              Nothing -> done
                              Just userInfo1 -> 
                                 reallyCheckOutNode userInfo1 version0
                     else
                        errorMess 
                           ("Version is not checked into this repository\n"
                           ++ "(It may be a parent version from another "
                           ++ "repository)")
                  )

         reallyCheckOutNode :: UserInfo -> Version -> IO ()
         reallyCheckOutNode userInfo version0 =
            do
               view <- getView repository graphClient version0
               setUserInfo view userInfo
               versionGraphNode <- newWorkingVersion graphClient view

               (Just displayedView) <- openGeneralDisplay
                  displaySort FolderDisplayType view

               addCloseDownAction displayedView (
                  deleteWorkingVersion graphClient view
                  )
               done

         -- Edit the view information for a working version
         editWorkingNode :: VersionInfo1 -> IO ()
         editWorkingNode versionInfo1 =
            doOp (
               do
                  let
                     Just view = toViewOpt versionInfo1
                     versionInfo0 = toVersionInfo versionInfo1
                  userInfo1Opt <- editVersionInfo  "Edit Info" versionInfo0
                  case userInfo1Opt of
                     Nothing -> done
                     Just userInfo1 -> setUserInfo view userInfo1
                )

         -- View the view information for a working version
         viewWorkingNode :: VersionInfo1 -> IO ()
         viewWorkingNode versionInfo1 =
            doOp (
               do
                  forkIODebug (displayVersionInfo False (
                     toVersionInfo versionInfo1))
                  done
               )

         -- Edit the view information for a checked-in version
         editCheckedInNode :: VersionInfo1 -> IO ()
         editCheckedInNode versionInfo1 =
            doOp (
               do
                  let 
                     versionInfo0 = toVersionInfo versionInfo1
                  userInfo1Opt <- editVersionInfo "Edit Info" versionInfo0
                  case userInfo1Opt of
                     Nothing -> done
                     Just userInfo1 -> modifyUserInfo repository userInfo1
               )
     
         -- Delete or undelete node
         mDelCheckedInNode :: (VersionInfo -> WithError VersionInfo) 
            -> VersionInfo1 -> IO ()
         mDelCheckedInNode delFn versionInfo1 =   
            doOp (
               do
                  let 
                     versionInfo0 = toVersionInfo versionInfo1
                     versionInfo1WE = delFn versionInfo0
                  case fromWithError versionInfo1WE of
                     Right versionInfo1 ->
                        modifyUserInfo repository (user versionInfo1)
                     Left mess -> errorMess mess
               )


         -- Display the view information for a checked-in version
         viewCheckedInNode :: VersionInfo1 -> IO ()
         viewCheckedInNode versionInfo1 =
            doOp (displayVersionInfo True (toVersionInfo versionInfo1))

         -- Extract the title for a node
         nodeTitle :: VersionInfo1 -> IO String
         nodeTitle = return . versionInfoTitle . toVersionInfo 

         versionInfoTitle :: VersionInfo -> String
         versionInfoTitle versionInfo0 =
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
            in
               identifier2

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

               listBox <- newSimpleListBox topLevel versionInfoTitle []

               confirmFrame <- newFrame topLevel []

               okButton <- newButton confirmFrame [text "OK"]
               cancelButton <- newButton confirmFrame [text "Cancel"]

               okClicked <- clicked okButton
               cancelClicked <- clicked cancelButton
               (cancelButtonDestroyed,_) <- bindSimple 
                  cancelButton Destroy 

               pack okButton [Side AtLeft]
               pack cancelButton [Side AtRight]
               pack messageWindow [Side AtTop]
               pack listBox [Side AtTop]
               pack confirmFrame [Side AtTop]

               -- Construct event for delete actions in this list.
               (deleteEvent :: Event [SimpleListBoxItem VersionInfo],
                  terminator) <- bindSelection listBox

               let
                  mkMergeCandidate :: WrappedNode node 
                     -> IO (Maybe VersionInfo)
                  mkMergeCandidate (WrappedNode node0) =
                     do
                        mergeNode0 <- getNodeValue dispGraph node0
                        let
                           ((_,versionInfo1) :: (Node,VersionInfo1)) 
                              = dynCast "VersionGraph.mkMergeCandidate"
                                 mergeNode0

                           versionInfo = toVersionInfo versionInfo1

                        case (toViewOpt versionInfo1,isPresent versionInfo) of
                           (Just _,_) -> -- must be a view.
                              do
                                 errorMess 
                                    "You may only select checked-in versions"
                                 return Nothing
                           (_,False) ->
                              do
                                 errorMess 
                                    "Version not present in this repository"
                                 return Nothing
                           _ -> return (Just versionInfo) 

                  -- This function is passed to getMultipleNodes for the
                  -- graph and returns the nodes to merge, or Nothing
                  -- if the operation is to be cancelled.
                  getNodes :: Event (WrappedNode node) 
                     -> IO (Maybe [VersionInfo])
                  getNodes newVersion =
                     do
                         let
                            -- Now here is the event for the business of
                            -- getNodes 
                            getNodesEvent :: Event (Maybe [VersionInfo])
                            getNodesEvent =
                                  (do
                                     okClicked
                                     always (
                                        do
                                           versionInfos <- getItems
                                              listBox
                                           return (Just versionInfos)
                                        )
                                  )
                               +> (do
                                     cancelClicked
                                     return Nothing
                                  )
                               +> (do
                                     cancelButtonDestroyed
                                        -- This escapes if the user
                                        -- destroys the selection window.
                                     return Nothing
                                     )
                               +> (do
                                     (selection :: WrappedNode node)
                                        <- newVersion
                                     always (
                                        do
                                           versionInfoOpt <- 
                                              mkMergeCandidate selection
                                           case versionInfoOpt of
                                              Just versionInfo ->
                                                 do
                                                    addItemAtEnd listBox 
                                                       versionInfo
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
               versionInfosOpt <- getMultipleNodes dispGraph getNodes

               -- Clean up
               terminator
               destroy topLevel

               -- Get object versions out
               let
                  objectVersionsOpt = case versionInfosOpt of
                     Nothing -> Nothing
                     Just versionInfos -> Just (
                        map  (version . user) versionInfos) 
               return objectVersionsOpt

         -- Function to do when the user asks to copy versions.
         copyVersions1 :: IO ()
         copyVersions1 =
            doOp (
               do
                  versionGraph <- readMVar versionGraphMVar
                  copyVersions versionGraph
               )
                 
         -- Function to be executed when the user requests a merge.
         -- We can only merge checked-in versions.
         doMerge :: IO ()
         doMerge =
            doOp (
               do
                  objectVersionsOpt 
                     <- selectCheckedInVersions "Versions to Merge"
                  case objectVersionsOpt of
                     Nothing -> done
                     Just [] -> errorMess "No versions specified!"
                     Just [_] -> errorMess "Only one version specified"
                     Just objectVersions ->
                        do
                           -- Go ahead.
                           viewWE <- mergeNodes repository graphClient (
                              map Right objectVersions)
                           case fromWithError viewWE of
                              Left mess -> errorMess mess
                              Right view ->
                                 do
                                    versionInfo0 <- readVersionInfo view
                                    userInfo1Opt <- editVersionInfo
                                       "Alter Merge Info" versionInfo0
                                    case userInfo1Opt of
                                       Nothing -> done
                                       Just userInfo1 ->
                                          setUserInfo view userInfo1
                                    newWorkingVersion graphClient view

                                    (Just displayedView) <- openGeneralDisplay
                                       displaySort FolderDisplayType view

                                    addCloseDownAction displayedView (
                                       deleteWorkingVersion graphClient view
                                       )
                                    done
               )                                          

      -- Construct the graph
      (displayedGraph,dispGraph) <- displayGraph1 displaySort 
            (toVersionGraphConnection graphClient) graphParms
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
            graph = graphClient,
            closeDownAction = closeDownAction,
            closedEvent = receive destroyedChannel,
            repository = repository,
            selectCheckedInVersions = selectCheckedInVersions
            }

      putMVar versionGraphMVar versionGraph
      -- And return it
      return versionGraph

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

toVersionGraphRepository :: VersionGraph -> Repository
toVersionGraphRepository = repository

toVersionGraphClient :: VersionGraph -> VersionGraphClient
toVersionGraphClient = graph


-- --------------------------------------------------------------------
-- VersionInfoFilter operations
-- --------------------------------------------------------------------

setNewFilter :: VersionGraphClient -> IO ()
setNewFilter graphClient =
   do
      filterOpt <- readVersionInfoFilter
      case filterOpt of
         Nothing -> done
         Just newFilter -> setNewFilter0 graphClient newFilter

-- --------------------------------------------------------------------
-- Committing.
-- --------------------------------------------------------------------

commitViewInGraph :: View -> IO ()
commitViewInGraph view =
   do
      versionInfo <- readVersionInfo view
      commitVersionInfo2 versionInfo view
             
commitVersionInfo1 :: VersionInfo1 -> IO ()
commitVersionInfo1 versionInfo1 =
   do
      let
         Just view = toViewOpt versionInfo1
         versionInfo0 = toVersionInfo versionInfo1
      commitVersionInfo2 versionInfo0 view

commitVersionInfo2 :: VersionInfo -> View -> IO ()
commitVersionInfo2 versionInfo0 view=
   doOp (
      do
         userInfo1Opt <- editVersionInfo
            "Commit Version" versionInfo0
         case userInfo1Opt of
            Nothing -> done
            Just userInfo1 -> reallyCommitNode userInfo1 view
      )

reallyCommitNode :: UserInfo -> View -> IO ()
reallyCommitNode userInfo view =
   do
      setUserInfo view userInfo
      commitView view
      done

-- --------------------------------------------------------------------
-- We globally lock all version graph operations (for now).  I
-- know this is unnecessarily strict; perhaps one day we will make
-- this more fine-grained.
-- --------------------------------------------------------------------

lock :: BSem
lock = unsafePerformIO newBSem
{-# NOINLINE lock #-}

-- If we try to do something when the lock is already acquired,
-- we simply give up
doOp :: IO () -> IO ()
doOp act =
   do   
      acquired <- tryAcquire lock
      if acquired 
         then
            finally act (release lock)
         else 
            putStrLn ("VersionGraph: attempt to modify graph when "
               ++ "it is already being modified ignored")


    
   

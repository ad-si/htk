{- This module contains the code which connects to the VersionInfo service
   (defined in simpledb) and constructs a graph.  It also defines 
   functions for packing and unpacking Node values. -}
module VersionGraphClient(
   versionGraph, -- :: VersionTypes SimpleGraph
   VersionTypes,

   -- Node functions
   VersionGraphNode(..),
   toNode, -- :: VersionGraphNode -> Node
   versionToNode, -- :: ObjectVersion -> Node
   nodeToVersion, -- :: Node -> Maybe Version
   nodeIsCheckedIn, -- :: Node -> Bool

   -- Arc functions 
   newCheckedInArc, -- :: VersionTypes SimpleGraph -> IO Arc
   newWorkingArc, -- :: UniqueStringSource -> IO Arc
   arcIsCheckedIn, -- :: Arc -> Bool

 
   -- The pre-defined types.
   checkedInType, workingType, -- :: NodeType
   checkedInArcType, workingArcType, -- :: ArcType

   getVersionInfo, -- :: Node -> IO VersionInfo
      -- Get the version Info for a node.
   ) where

import System.IO.Unsafe

import AtomString
import UniqueString
import Computation
import Spawn
import NewNames
import BinaryIO
import Debug

import InfoBus

import CallServer

import Graph
import SimpleGraph

import VersionInfo
import VersionInfoService

import VersionDB
import ViewType

-- ------------------------------------------------------------------------
-- Types
-- ------------------------------------------------------------------------

---
-- A VersionGraphNode represents either a checked-in version or a view.
-- Only the checked-in nodes are sent to the server.
data VersionGraphNode =
      CheckedInNode ObjectVersion
   |  WorkingNode View

type VersionTypes dataSort = dataSort VersionInfo () () () 
   -- change to alter node/nodetype/arc/arctypes.
   -- The NodeType uses as String the string representation of
   -- the version.  Thus each node has attached to it the NodeType
   -- plus the additional String which is supplied; this String
   -- can be used to provide additional information about the version.

type VersionGraph = VersionTypes SimpleGraph

-- ------------------------------------------------------------------------
-- Connecting to the Server
-- ------------------------------------------------------------------------

versionGraph :: VersionTypes SimpleGraph
versionGraph = unsafePerformIO mkVersionGraph
{-# NOINLINE mkVersionGraph #-}

---
-- Get the VersionGraph, and arrange for the connection to be closed
-- when the program ends.
mkVersionGraph :: IO (VersionTypes SimpleGraph)
mkVersionGraph =
   do
      (versionGraph,terminator) <- connectToServer
      registerDestroyAct terminator
      return versionGraph

---
-- retrieve the VersionInfo corresponding to a given version.
getVersionInfo :: Node -> IO VersionInfo
getVersionInfo node = getNodeLabel versionGraph node


---
-- connectToServer generates a new graph connected to the version
-- graph in the server.  Updates to this graph do not get passed to
-- the server.
-- The returned action closes the server connection.
connectToServer :: IO (VersionTypes SimpleGraph,IO ())
connectToServer =
   do
      (getNextUpdate,closeConnection,initialVersionInfos) 
          <- connectExternal versionInfoService

      (graph :: VersionTypes SimpleGraph) <- newEmptyGraph

      debug initialVersionInfos

      update graph (NewNodeType checkedInType ())
      update graph (NewNodeType workingType ())
      update graph (NewArcType checkedInArcType ())
      update graph (NewArcType workingArcType ())

      let
         -- The action returned adds the arcs, if this is a new node.
         -- It needs to be done after all new nodes have been added.
         addVersionInfo :: (Bool,VersionInfo) -> IO (IO ())
         addVersionInfo (isModify,versionInfo) =
            do
               let
                  node = versionToNode (version (user versionInfo))

               if isModify
                 then
                    do
                       update graph (SetNodeLabel node versionInfo)
                       -- For now we do *not* modify the parents shown,
                       -- if VersionInfo says so.  The user shouldn't be able
                       -- to provoke that anyway.
                       return done
                 else
                    do
                       update graph (NewNode node checkedInType versionInfo)
                       let
                          act = 
                           mapM_
                              (\ parent ->
                                 newArc graph checkedInArcType () 
                                    (versionToNode parent) node
                                 ) 
                              (parents (user versionInfo))
                       return act

      acts <- mapM 
         (\ versionInfo -> addVersionInfo (False,versionInfo))
         initialVersionInfos

      sequence_ acts

      let
         updateThread =
            do
               versionInfo <- getNextUpdate
               debug versionInfo
               act <- addVersionInfo versionInfo
               act
               updateThread

               -- If closeConnection is executed (as it should be, when the
               -- versionGraph window is closed) the following things should
               -- happen: (1) GHC closes the Handle containing updates from
               -- the server; (2) GHC notices that getNextUpdate is never 
               -- going to be fulfilled and updateThread is therefore
               -- permanently blocked; (3) the updateThread is silently
               -- killed and garbage-collected.  This means "graph" may
               -- also be GC'd, if no-one else is currently referencing it.

      spawn updateThread

      return (graph,closeConnection)

-- -----------------------------------------------------------------------
-- The NodeType/ArcType values.  Clients should not define any others.
-- -----------------------------------------------------------------------

checkedInType :: NodeType
checkedInType = fromString "C"

workingType :: NodeType
workingType = fromString "W"

checkedInArcType :: ArcType
checkedInArcType = fromString "C"

workingArcType :: ArcType
workingArcType = fromString "W"

-- -----------------------------------------------------------------------
-- Functions for packing and unpacking Node and Arc values.
-- -----------------------------------------------------------------------


---
-- Each Node contains a String.  This Node has the following format:
-- "C[version]" for checked-in versions
-- "W[viewid]" for views.
-- Map a VersionGraphNode to the corresponding node
toNode :: VersionGraphNode -> Node
toNode (CheckedInNode version) = fromString . ('C' :) . toString $ version
toNode (WorkingNode view) = fromString . ('W' :) . show . viewId $ view

---
-- Returns the node associated with a version
versionToNode :: ObjectVersion -> Node
versionToNode version = toNode (CheckedInNode version)

---
-- Returns the version associated with a node, if it is checked in.
nodeToVersion :: Node -> Maybe ObjectVersion
nodeToVersion node =
   case toString node of
      'C' : versionString -> Just (fromString versionString)
      _ -> Nothing

---
-- Check if node is checked in
nodeIsCheckedIn :: Node -> Bool
nodeIsCheckedIn node = 
   case toString node of
      'C' : versionString -> True
      _ -> False


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




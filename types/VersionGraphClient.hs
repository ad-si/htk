{- This module contains the code which connects to the Version Graph
   (defined in VersionGraphService) and functions for packing and unpacking
   Node values. -}
module VersionGraphClient(
   versionGraph, -- :: VersionTypes SimpleGraph

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

 

   ) where

import System.IO.Unsafe

import AtomString
import UniqueString
import Computation
import Spawn
import NewNames

import InfoBus

import CallServer

import Graph
import SimpleGraph

import VersionDB
import ViewType
import View (Version)
import VersionGraphService

-- ------------------------------------------------------------------------
-- Types
-- ------------------------------------------------------------------------

---
-- A VersionGraphNode represents either a checked-in version or a view.
-- Only the checked-in nodes are sent to the server.
data VersionGraphNode =
      CheckedInNode Version
   |  WorkingNode View


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
      

---
-- Filter an Update to discover if it should be sent to the server.
-- A large number of operations are forbidden.
filterUpdate :: VersionTypes Update -> Bool
filterUpdate (NewNode node nodeType nodeLabel) = nodeIsCheckedIn node
filterUpdate (DeleteNode node) = nodeIsCheckedIn node
filterUpdate (NewArc arc arcType arcLabel nodeFrom nodeTo) =
   arcIsCheckedIn arc
filterUpdate (DeleteArc arc) = arcIsCheckedIn arc
filterUpdate (SetNodeLabel node _) = nodeIsCheckedIn node
filterUpdate update = 
   error ("VersionGraph error: update "++show update++" not handled")


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
nodeToVersion :: Node -> Maybe Version
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




{- #########################################################################

MODULE        : DaVinciSelection
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : DaVinciDrag and Drop Utility.


   ######################################################################### -}


module DaVinciDragAndDrop (
        Node,
        Edge,
        Graph,

        Toggle(..),
        dragging,
                
        createNodeGesture,
        createChildGesture,
        createEdgeGesture
        
        ) where

import SIM

import DaVinciCore
import Resources(Toggle(..))
import DaVinciEvent
import DaVinciNode
import DaVinciEdge
import DaVinciGraph

import Debug(debug)



-- ---------------------------------------------------------------------------
--  Set Drag On/Off
-- ---------------------------------------------------------------------------

dragging :: Toggle -> Config Graph
dragging On g   = withGraph g (return "drag_and_drop(dragging_on)")
dragging Off g  = withGraph g (return "drag_and_drop(dragging_off)")



-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

createNodeGesture :: Graph -> IA ()
createNodeGesture g  = listenDaVinci (g,CreateNode) >>> done


createChildGesture :: Graph -> IA Node
createChildGesture g  = 
        listenDaVinci (g,CreateNodeAndEdge) >>>= \(NodeSelection [nid]) -> 
                getNode g nid
                

createEdgeGesture :: Graph -> IA (Node,Node)
createEdgeGesture g  = 
        listenDaVinci (g,CreateEdge) >>>= \(NodeSelection [sid,tid]) -> do {
                src <- getNode g sid;
                trg <- getNode g tid;
                return (src,trg)
                }               



                

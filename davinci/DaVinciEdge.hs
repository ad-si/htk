{- #########################################################################

MODULE        : DaVinciEdge
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Edges

   ######################################################################### -}


module DaVinciEdge (
        Edge(..),
        newEdge,

        EdgeId(..),
        getEdge,

        ancestor,
        child,

        DaVinciEdge(..),
        Pattern(..),
        ArrowHead(..),

        hideEdges,
        restoreAllEdges,
        showEdges,

        existEdge,
        getSource,
        getTarget,

        edgeNotFound
        ) where

import SIM
import GUICore
import DaVinciEdgeType
import DaVinciClasses
import DaVinciGraph
import DaVinciNode
import DaVinciCore
import DaVinciGraphTerm
import Char(isSpace)

import Debug(debug)


-- ---------------------------------------------------------------------------
--  Edge Creation
-- ---------------------------------------------------------------------------

newEdge :: Maybe String -> Node -> Node -> [Config Edge] -> IO Edge
newEdge _ n1 n2 _ | (fNodeContext n1) /= (fNodeContext n2) = 
        raise edgeDomainError
newEdge meid src trg confs =
        synchronize g (do {
                gobj <- newAbstractGUIObject;
                eid <- newEdgeId g meid;
                ed <- configure (Edge g (TypeId "") eid gobj) confs;
                createEdge g ed src trg;
                return ed
                }) where g = fNodeContext src

edgeDomainError :: IOError
edgeDomainError = 
        userError "the source and target node of the edge belongs to different graphs"  



-- ---------------------------------------------------------------------------
--  Child Creation
-- ---------------------------------------------------------------------------

child :: Maybe String -> [Config Node] -> Config Node
child mnid confs parent = 
   synchronize g (do {
        ch <- newNode g mnid confs ; 
        newEdge Nothing parent ch []; 
        return parent
        }) where g = fNodeContext parent

ancestor :: Node -> [Config Node] -> IO Node
ancestor parent confs = synchronize g (do {
        ch <- newNode g Nothing confs; 
        newEdge Nothing parent ch []; 
        return ch
        }) where g = fNodeContext parent


-- ---------------------------------------------------------------------------
--  Edge Instances
-- ---------------------------------------------------------------------------

instance Object Edge where
        objectID = objectID . fEdgeObject
        
instance GUIObject Edge where
        toGUIObject e  = fEdgeObject e
        cname _        = "Edge"
        cset e cid val = do {setEdgeAttr e cid val; return e} 
        cget e cid     = getEdgeAttr e cid


instance Destructible Edge where
        destroy e   = delEdge e
        destroyed _ = inaction                          -- TBD


instance DaVinciObject Edge where
        getGraphContext e = return (fEdgeContext e)
        getDaVinciObjectID (Edge _ _ (EdgeId nm) _) = return nm 


instance TypedDaVinciObject Edge where
        getTypeName (Edge _ (TypeId tnm) _ _) = return tnm
        typename tnm (Edge g _ eid gobj) = do {
                tnm' <- toEdgeTypeDesignator (g,tnm);
                return (Edge g (TypeId tnm) eid gobj)
                }


instance HasColour Edge where
        setColour edge cid col  = cset edge "EDGECOLOR" col
        getColour edge cid      = cget edge "EDGECOLOR"


instance DaVinciEdge Edge

instance Synchronized Edge where
        synchronize w = synchronize (toGUIObject w)


-- ---------------------------------------------------------------------------
--  Edge Commands: hide, restore, show
-- ---------------------------------------------------------------------------

hideEdges :: [Edge] -> IO ()                    
hideEdges [] = done
hideEdges eids @ (e : l)= do {
        withGraph g (return ("menu(abstraction(hide_edges("++show eids'++")))"));
        done
} where eids' = toEdgeIds eids
        g     = fEdgeContext e


restoreAllEdges :: Graph -> IO ()
restoreAllEdges g = do {
        withGraph g (return ("menu(abstraction(restore_all_edges))"));
        done
        }


showEdges ::  [Edge] -> IO ()
showEdges [] = done
showEdges eids @ (e : l) = do {
        withGraph g (
                return ("menu(abstraction(show_edges("++show eids'++")))"));
        done
} where eids' = toEdgeIds eids
        g     = fEdgeContext e


toEdgeIds eids = map (\(Edge g _ eid _) -> eid) eids


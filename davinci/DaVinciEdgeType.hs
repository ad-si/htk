{- #########################################################################

MODULE        : DaVinciEdgeType
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module DaVinciEdgeType (
        module DaVinciClasses,

        EdgeTypeDesignator(..), 

        getDefaultEdgeType,

        EdgeType,
        newEdgeType,

        edgetype,
        getEdgeType

        ) where

import HTk
import Font
import GUICore

import Char(isSpace,toLower)

import DaVinciCore
import DaVinciGraphTerm
import DaVinciClasses
import DaVinciMenu

import Debug(debug)


-- ---------------------------------------------------------------------------
--  DaVinci Type Designator
-- ---------------------------------------------------------------------------

class EdgeTypeDesignator t where
        toEdgeTypeDesignator :: t -> IO EdgeType

instance EdgeTypeDesignator EdgeType where
        toEdgeTypeDesignator = return . id

instance EdgeTypeDesignator (Graph,[Char]) where
        toEdgeTypeDesignator (g,tnm) = do {
                o <- getType g tid;
                return (EdgeType g o tid)
                } where tid = TypeId tnm


-- ---------------------------------------------------------------------------
--  Data Type
-- ---------------------------------------------------------------------------

data EdgeType = EdgeType Graph GUIOBJECT TypeId deriving Eq             


-- ---------------------------------------------------------------------------
--  Constructor
-- ---------------------------------------------------------------------------

newEdgeType :: Graph -> Maybe String -> [Config EdgeType] -> IO EdgeType
newEdgeType g mtnm confs = do {
        (tnm,o) <- newType g mtnm;
        configure (EdgeType g o tnm) confs
        }

-- ---------------------------------------------------------------------------
--  Default Node Type
-- ---------------------------------------------------------------------------

getDefaultEdgeType :: Graph -> IO EdgeType
getDefaultEdgeType g = toEdgeTypeDesignator (g,"")


-- ---------------------------------------------------------------------------
--  Instantiations
-- ---------------------------------------------------------------------------

instance Object EdgeType where
        objectID (EdgeType _ obj tnm) = objectID obj
        
instance GUIObject EdgeType where
        toGUIObject (EdgeType g o tnm) = o
        cname _ = "EdgeType"
        cset et@(EdgeType g o tnm) cid v = synchronize et (do {
                cset obj cid val;
                attrs <- lookupConfigs obj;
                withGraph g ( return (
                        "visual(add_rules([er(" ++ show tnm ++ "," ++
                        show (assocs attrs) ++ ")]))"
                        ));
                return et
                }) where val = toGUIValue v
                         obj = toGUIObject et 
                         assocs = map (\(cid,val) -> AttrAssoc cid val) 

instance Destructible EdgeType where
        destroy et@(EdgeType g o tnm) = synchronize et (do {delType g tnm})
        destroyed (EdgeType g o tnm) = destroyed o


instance HasMenu EdgeType where
        menu mn et@(EdgeType g _ tid) = synchronize et (do {
                attrs <- lookupConfigs (toGUIObject et);                
                installEdgeTypeMenu mn g tid (assocs attrs);
                return et
                }) where assocs = map (\(cid,val) -> AttrAssoc cid val)

instance DaVinciObject EdgeType where
        getGraphContext (EdgeType g _ _) = return g
        getDaVinciObjectID (EdgeType g _ (TypeId tnm)) = return tnm


instance Synchronized EdgeType where
        synchronize nt = synchronize (toGUIObject nt)

instance DaVinciEdge EdgeType

instance HasColour EdgeType where
        setColour nt cid col    = cset nt "EDGECOLOR" col
        getColour nt cid        = cget nt "EDGECOLOR"

-- ---------------------------------------------------------------------------
--  Configure Options
-- ---------------------------------------------------------------------------

edgetype :: EdgeTypeDesignator t => t -> Config Edge
edgetype t (Edge g _ eid gobj) = do {
        (EdgeType g _ tnm) <- toEdgeTypeDesignator t;
        return (Edge g tnm eid gobj)
        }

getEdgeType :: Edge -> IO EdgeType
getEdgeType (Edge g (TypeId tnm) eid _) = 
        toEdgeTypeDesignator (g,tnm)
        

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
   configEdgeTypeMenu, 
       -- :: GraphConfigure.LocalMenu Edge -> EdgeType -> IO EdgeType
   
   edgetype,
   getEdgeType
   
   ) where

import Char(isSpace,toLower)

import HTk
import Font
import GUICore

import qualified GraphConfigure

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
-- Install Menus
-- ---------------------------------------------------------------------------

configEdgeTypeMenu :: GraphConfigure.LocalMenu Edge -> EdgeType -> IO EdgeType
configEdgeTypeMenu (GraphConfigure.LocalMenu menuPrim) 
      edgeType@(EdgeType graph _ typeId) =
   let
      menuPrimId = -- menu with EdgeId -> IO() rather than Edge -> IO ().
         GraphConfigure.mapMenuPrim
            (\ action ->
               let
                  actionId edgeId =
                     do
                        edge <- getEdge graph edgeId
                        action edge
               in
                  actionId
               )
            menuPrim
      localId = GraphConfigure.LocalMenu menuPrimId
   in
      synchronize edgeType (
         do
            attributes <- lookupConfigs (toGUIObject edgeType)
            let
               assocs = map (\ (cid,val) -> AttrAssoc cid val) attributes
            installEdgeTypeMenu localId graph typeId assocs
            return edgeType
         )


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
        



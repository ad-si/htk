{- #########################################################################

MODULE        : DaVinciNodeType
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module DaVinciNodeType (
   module DaVinciClasses,
   
   NodeTypeDesignator(..), 
   
   getDefaultNodeType,
   
   NodeType,
   newNodeType,
   configNodeTypeMenu, -- :: LocalMenu Node -> NodeType -> IO NodeType

   nodetype,
   getNodeType,
   
   illegalDaVinciBitMap
   
   ) where

import Char(isSpace,toLower)

import HTk
import GUICore
import Font
import BitMap

import qualified GraphConfigure

import Line(ArrowHead(..))

import DaVinciCore
import DaVinciGraphTerm
import DaVinciClasses
import DaVinciMenu

import Debug(debug)



-- ---------------------------------------------------------------------------
--  DaVinci Type Designator
-- ---------------------------------------------------------------------------

class NodeTypeDesignator t where
        toNodeTypeDesignator :: t -> IO NodeType

instance NodeTypeDesignator NodeType where
        toNodeTypeDesignator = return . id

instance NodeTypeDesignator (Graph,[Char]) where
        toNodeTypeDesignator (g,tnm) = do {
                o <- getType g tid;
                return (NodeType g o tid)
                } where tid = TypeId tnm


-- ---------------------------------------------------------------------------
--  Data Type
-- ---------------------------------------------------------------------------

data NodeType = NodeType Graph GUIOBJECT TypeId deriving Eq             


-- ---------------------------------------------------------------------------
--  Constructor
-- ---------------------------------------------------------------------------

newNodeType :: Graph -> Maybe String -> [Config NodeType] -> IO NodeType
newNodeType g mtnm confs = do {
        (tnm,o) <- newType g mtnm;
        configure (NodeType g  o tnm) confs
        }


-- ---------------------------------------------------------------------------
--  Default Node Type
-- ---------------------------------------------------------------------------

getDefaultNodeType :: Graph -> IO NodeType
getDefaultNodeType g = toNodeTypeDesignator (g,"")


-- ---------------------------------------------------------------------------
--  Instantiations
-- ---------------------------------------------------------------------------

instance Object NodeType where
        objectID (NodeType _ obj tnm) = objectID obj
        
instance GUIObject NodeType where
        toGUIObject (NodeType _ obj tnm) = obj
        cname _ = "NodeType"
        cset nt@(NodeType g o tnm) cid v = synchronize nt (do {
                cset obj cid val;
                attrs <- lookupConfigs obj;
                withGraph g ( return (
                        "visual(add_rules([nr(" ++ show tnm ++ "," ++
                        show (assocs attrs) ++ ")]))"
                        ));
                return nt
                }) where val = toGUIValue v
                         obj = toGUIObject nt 
                         assocs = map (\(cid,val) -> AttrAssoc cid val) 


instance Destructible NodeType where
        destroy nt@(NodeType g o tnm) = synchronize nt (do {delType g tnm})
        destroyed (NodeType g o tnm) = destroyed o


instance DaVinciObject NodeType where
        getGraphContext (NodeType g _ _) = return g
        getDaVinciObjectID (NodeType _ _ (TypeId tnm)) = return tnm

instance Synchronized NodeType where
        synchronize nt = synchronize (toGUIObject nt)

instance DaVinciNode NodeType

instance HasColour NodeType where
        setColour nt cid col    = cset nt "COLOR" col
        getColour nt cid        = cget nt "COLOR"

instance HasBitMap NodeType where
        bitmap b n = setBMap (toBitMap b) n
          where setBMap :: BitMapHandle -> Config NodeType
                setBMap (Predefined _) n = raise illegalDaVinciBitMap
                setBMap (BitMapHandle _) n = raise illegalDaVinciBitMap
                setBMap (BitMapFile fname) n = 
                        synchronize n (do {
                                cset n "_GO" (RawData "icon");
                                cset n "ICONFILE" (RawData fname) 
                                })
        getBitMap n = do {
                (RawData str) <- cget n "ICONFILE";
                (return . BitMapFile) str
                }

-- ---------------------------------------------------------------------------
-- Install Menus
-- ---------------------------------------------------------------------------

configNodeTypeMenu :: GraphConfigure.LocalMenu Node -> NodeType -> IO NodeType
configNodeTypeMenu (GraphConfigure.LocalMenu menuPrim) 
      nodeType@(NodeType graph _ typeId) =
   let
      menuPrimId = -- menu with NodeId -> IO() rather than Node -> IO ().
         GraphConfigure.mapMenuPrim
            (\ action ->
               let
                  actionId nodeId =
                     do
                        node <- getNode graph nodeId
                        action node
               in
                  actionId
               )
            menuPrim
      localId = GraphConfigure.LocalMenu menuPrimId
   in
      synchronize nodeType (
         do
            attributes <- lookupConfigs (toGUIObject nodeType)
            let
               assocs = map (\ (cid,val) -> AttrAssoc cid val) attributes
            installNodeTypeMenu localId graph typeId assocs
            return nodeType
         )

-- ---------------------------------------------------------------------------
--  Configure Options
-- ---------------------------------------------------------------------------

nodetype :: NodeTypeDesignator t => t -> Config Node
nodetype t (Node g _ nid gobj) = do {
        (NodeType g _ tnm) <- toNodeTypeDesignator t;
        return (Node g tnm nid gobj)
        }

getNodeType :: Node -> IO NodeType
getNodeType (Node g (TypeId tnm) nid _) = 
        toNodeTypeDesignator (g,tnm)
        

-- ---------------------------------------------------------------------------
--  Bitmaps
-- ---------------------------------------------------------------------------

illegalDaVinciBitMap :: IOError
illegalDaVinciBitMap = 
        userError "a daVinci bitmap designator must be the name of a file"



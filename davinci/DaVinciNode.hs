{- #########################################################################

MODULE        : DaVinciNode
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Nodes


   ######################################################################### -}


module DaVinciNode (
        BitMap,
        HasBitMap(..),
        BitMapHandle,
        BitMapDesignator(..),

        Node(..),
        newNode,

        NodeId(..),
        getNode,

        DaVinciNode(..),
        Border(..),
        Shape(..),
        FontFamily(..),
        FontSlant(..),

        hideSubgraph,
        showSubgraph, 
        deleteSubgraph,

        focusNode,
        focusNodeAnimated,

        existNode,
        getParents,
        getAncestors,
        getChildren,
        getSiblings,
        getIncoming,
        getOutgoing,
        isRootNode,

        illegalDaVinciBitMap,
        nodeNotFound    
        ) where

import SIM
import GUICore
import BitMap
import DaVinciCore
import DaVinciClasses
import DaVinciGraph
import DaVinciGraphTerm
import DaVinciNodeType
import Char(isLower,isSpace,toLower)


import Debug(debug)



-- ---------------------------------------------------------------------------
--  Create Node
-- ---------------------------------------------------------------------------

newNode :: Graph -> Maybe String -> [Config Node] -> IO Node
newNode g mnid confs = synchronize g (do {
        nid <- newNodeId g mnid;
        gobj <- newAbstractGUIObject;
        nd <- configure (Node g (TypeId "") nid gobj) confs;
        createNode g nd;
        return nd
        })

-- ---------------------------------------------------------------------------
--  Node Instances
-- ---------------------------------------------------------------------------

instance Object Node where
        objectID = objectID . fNodeObject
        
instance GUIObject Node where 
         toGUIObject n = fNodeObject n
         cname _ = "Node"
         cset n cid val = do {setNodeAttr n cid val; return n}
         cget n cid = getNodeAttr n cid

instance Destructible Node where
        destroy n       = delNode n
        destroyed _     = inaction                      -- TBD


instance DaVinciObject Node where
        getGraphContext = return . fNodeContext
        getDaVinciObjectID (Node _ _ (NodeId tnm) _) = return tnm


instance TypedDaVinciObject Node where
        getTypeName (Node _ (TypeId tnm) _ _) = return tnm
        typename tnm (Node g _ nid gobj) = do {
                tnm' <- toNodeTypeDesignator (g,tnm);
                return (Node g (TypeId tnm) nid gobj)
                }

instance DaVinciNode Node

instance HasText Node String where
        text t n  = cset n "OBJECT" (RawData t)
        getText n = do {(RawData str) <- cget n "OBJECT"; return str}

instance HasColour Node where
        setColour n cid col     = cset n "COLOR" col
        getColour n cid         = cget n "COLOR"

instance HasBitMap Node where
        bitmap b n = setBMap (toBitMap b) n
          where setBMap :: BitMapHandle -> Config Node
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

instance Synchronized Node where
        synchronize w = synchronize (toGUIObject w)

-- ---------------------------------------------------------------------------
--  Show/View Nodes
-- ---------------------------------------------------------------------------

hideSubgraph :: [Node] -> IO ()
hideSubgraph [] = done
hideSubgraph nids @ (n : l) = do {
        withGraph g ( 
                return ("menu(abstraction(hide_subgraph("++show nids'++")))"));
        done
} where nids' = toNodeId nids
        g = fNodeContext n


showSubgraph ::  [Node] -> IO ()
showSubgraph [] = done
showSubgraph nids @ ((Node g _ _ _) : l) = do {
        withGraph g (
                return ("menu(abstraction(show_subgraph("++show nids'++")))"));
        done
} where nids' = toNodeId nids


deleteSubgraph :: Node -> IO ()
deleteSubgraph n = synchronize n (do {
        subgraphs <- getChildren n;
        exist <- existNode n;
        when exist (destroy n);
        foreach subgraphs deleteSubgraph;       
        done
        })

toNodeId =  map (\(Node g _ nid _) -> nid)

-- ---------------------------------------------------------------------------
--  Focus
-- ---------------------------------------------------------------------------

focusNode :: Node -> IO ()
focusNode n = do {
        withGraph g (return ("special(focus_node("++show nid++"))"));
        done
} where g   = fNodeContext n
        nid = fNodeId n



focusNodeAnimated :: Node -> IO ()
focusNodeAnimated n = do {
        withGraph g (return ("special(focus_node_animated("++show nid++"))"));
        done
} where g   = fNodeContext n
        nid = fNodeId n


-- ---------------------------------------------------------------------------
--  Graph Structure
-- ---------------------------------------------------------------------------

getParents :: Node -> IO [Node] 
getParents n = synchronize n (do {
        l <- getIncoming n;
        sequence (map getSource l)
        })

getAncestors :: Node -> IO [Node] 
getAncestors n = synchronize n (do {
        l <- getParents n; 
        al <- sequence (map getAncestors l);
        return (foldr (++) l al)
        })
        

getChildren :: Node -> IO [Node]
getChildren n = synchronize n (do {
        l <- getOutgoing n;
        sequence (map getTarget l)
        })

getSiblings :: Node -> IO [Node]
getSiblings n = synchronize n (do {
        l <- getParents n; 
        cl <- sequence (map getChildren l);
        return (joinList cl)
        }) where joinList [[]] = [] 
                 joinList (x:l) = x ++ joinList l


isRootNode :: Node -> IO Bool
isRootNode n = getIncoming n >>= return . (/= [])


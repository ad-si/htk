{- #########################################################################

MODULE        : DaVinciSelection
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : DaVinci Node Selection.


   ######################################################################### -}


module DaVinciSelection (
        Node,
        Edge,

        HasSelection(..),
        HasSelectionIndex(..),
        HasSelectionBaseIndex(..),

        selectChildren,
        selectParents,
        selectSiblings,
        selectSource,
        selectTarget,

        nodeSelected,
        edgeSelected,
        nodeDoubleClicked,
        edgeDoubleClicked

        ) where

import SIM
import Selection
import DaVinciCore
import DaVinciGraphTerm
import DaVinciNode(getChildren, getParents, getSiblings)
import DaVinciEvent
import DaVinciNode
import DaVinciEdge

import Debug(debug)




-- ---------------------------------------------------------------------------
--  Instantiations
-- ---------------------------------------------------------------------------

instance HasSelection Graph where
        clearSelection g = do
                withGraphOneWay g (return "special(select_nodes([]))")
                done  



-- ---------------------------------------------------------------------------
--  Node Selection
-- ---------------------------------------------------------------------------

instance HasSelectionIndex Graph [Node] where
        selection ns g = do {selectNodes ns; return g}
        isSelected g ns = do 
                sns <- getNodeSelection g
                return (all (\e -> elem e sns) ns)

instance HasSelectionBaseIndex Graph [Node] where
        getSelection g = do 
                snl <- getNodeSelection g
                case snl of
                        [] -> return Nothing
                        _  -> return (Just snl)


-- ---------------------------------------------------------------------------
--  Edge Selection
-- ---------------------------------------------------------------------------

instance HasSelectionIndex Graph Edge where
        selection ns g = do {selectEdge ns; return g}
        isSelected g e = do 
                se <- getEdgeSelection g
                case se of
                        [es] ->  return (e ==es)
                        _    ->  return False


instance HasSelectionBaseIndex Graph Edge where
        getSelection g = do 
                snl <- getEdgeSelection g
                case snl of
                        [] -> return Nothing
                        [e]  -> return (Just e)

         
instance HasSelectionIndex Graph [Edge] where
        selection [] g = return g
        selection (e:_) g = selection e g
        isSelected g [] = return False
        isSelected g (e:_) = isSelected g e

instance HasSelectionBaseIndex Graph [Edge] where
        getSelection g = do 
                snl <- getEdgeSelection g
                case snl of
                        [] -> return Nothing
                        [e]  -> return (Just [e])
         
-- ---------------------------------------------------------------------------
--  Primitive Commands
-- ---------------------------------------------------------------------------

selectNodes :: [Node] -> IO ()
selectNodes [] = done
selectNodes nids @ ((Node g _ _ _):_) = do {
        withGraphOneWay g (return ("special(select_nodes("++show nids' ++"))"));
        done
} where nids' = map (\(Node g _ nid _) -> nid) nids


selectEdge :: Edge -> IO ()
selectEdge (Edge g _ eid _) = do {
        withGraphOneWay g (return ("special(select_edge("++show eid++"))"));
        done
        }


-- ---------------------------------------------------------------------------
--  Derived Commands
-- ---------------------------------------------------------------------------

selectChildren :: Node -> IO () 
selectChildren n = synchronize n (do {
        cl <- getChildren n;
        selectNodes cl
        })
        
selectParents :: Node -> IO ()
selectParents n = synchronize n (do {
        pl <- getParents n;
        selectNodes pl
        })

selectSiblings :: Node -> IO ()
selectSiblings n = synchronize n (do {
        sl <- getSiblings n;
        selectNodes sl
        })

selectSource :: Edge -> IO ()
selectSource e = synchronize e (do { 
        s <- getSource e;
        selectNodes (replicate 1 s)
        })

selectTarget :: Edge -> IO ()
selectTarget e = synchronize e (do {
        t <- getTarget e;
        selectNodes  (replicate 1 t)
        })


-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

nodeSelected :: Graph -> IA [Node]
nodeSelected g  = 
        listenDaVinci (g,NodeSelectionLabels) >>>= \(NodeSelection nil) -> do {
                nl <- mapM (getNode g) nil;
                setNodeSelection g nl;
                return nl
                }


edgeSelected :: Graph -> IA Edge
edgeSelected g = 
        listenDaVinci (g,EdgeSelectionLabel) >>>= \(EdgeSelection eid) -> do {
                e <- getEdge g eid;
                setEdgeSelection g [e];
                return e
                }


nodeDoubleClicked :: Graph -> IA Node
nodeDoubleClicked g  = 
        listenDaVinci (g,NodeDoubleClick) >>>=  \ (NodeSelection [nid]) -> do {
                n <- getNode g nid;
                setNodeSelection g [n];
                return n
                }
                
edgeDoubleClicked :: Graph -> IA Edge
edgeDoubleClicked g = 
        listenDaVinci (g,EdgeDoubleClick) >>>= \(EdgeSelection eid) -> do {
                e <- getEdge g eid;
                setEdgeSelection g [e];
                return e
                }

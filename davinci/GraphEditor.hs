{- #########################################################################

MODULE        : GraphEditor
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : A GraphEditor


   ######################################################################### -}


module GraphEditor (
        newGraphEditor

        ) where

import qualified IOExts(unsafePerformIO)

import HTk
import DaVinci
import SIM


import PulldownMenu
import PromptWin
import ScrollBox
import DialogWin
import InputForm
import InputWin
import IconBar

import Debug(debug)


-- ---------------------------------------------------------------------------
-- Building the Graph Editor
-- ---------------------------------------------------------------------------

newGraphEditor :: DaVinci -> IO Graph
newGraphEditor dav = do
        g <- newGraph [
                gapwidth 4, 
                gapheight 40,
                text "Graph Editor",
                dragging On
                ]

        nmn <- newNodeMenu g
        newNodeType g (Just "NODE") [menu nmn]
        popupInterActor dav g nmn Nothing popupSelectionNode

        emn <- newEdgeMenu g
        newEdgeType g (Just "EDGE") [menu emn]
        popupInterActor dav g emn Nothing popupSelectionEdge

        ib <- newGraphIconBar g
        mn <- newGraphMenu g
        graphInterActor dav g ib mn 

        displayGraph g
        newSurveyView g
        
        return g

-- ---------------------------------------------------------------------------
-- Graph Monitor (Handling Selections and Drag&Drog Gestures)
-- ---------------------------------------------------------------------------

graphInterActor :: DaVinci -> Graph -> IconBar () -> Menu () -> IO InterActor
graphInterActor dav g ib mn = do
        mne <- getTrigger mn
        ibe <- getTrigger ib
        controller dav (\iact -> 
                nodeSelected g       >>> done
          +>    edgeSelected g       >>> done
          +>    nodeDoubleClicked g  >>> done
          +>    edgeDoubleClicked g  >>> done
          +>    createNodeGesture g  >>> createRoot g
          +>    createChildGesture g >>>= (createNode g)
          +>    createEdgeGesture g  >>>= (createEdge g)
          +>    destroyed g          >>> do {destroy mn; destroy ib; stop iact}
          +>    (mne +> ibe)         >>> done
          )


-- ---------------------------------------------------------------------------
-- Application Menus
-- ---------------------------------------------------------------------------

newNodeMenu :: Graph -> IO (AppMenu Node)
newNodeMenu g = do
        mn <- newMenu []
        newButton [text "Create", method createNode,parent mn]
        newButton [text "Delete", method deleteNode,parent mn]
        newButton [text "Edit", method editNode,parent mn]
        return mn

newEdgeMenu :: Graph -> IO (AppMenu Edge)
newEdgeMenu g = do
        mn <- newMenu []
        newButton [text "Delete", method deleteEdge,parent mn]
        newButton [text "Edit", method editEdge,parent mn]
        return mn

method :: Application a -> Config (Button (Application a))
method f = command (\() -> return f)



-- ---------------------------------------------------------------------------
-- Node Applications (Create, Delete, Edit)
-- ---------------------------------------------------------------------------

deleteNode :: Application Node
deleteNode g n = do {destroy n; redrawGraph g}

newGraphNode :: Graph -> IO Node
newGraphNode g = do
        n <- newNode g Nothing [nodetype (g,"NODE")]
        nd <- getNodeDefs
        setNodeDescr n nd
        configure n [nodename]

createRoot :: Graph -> IO ()
createRoot g = do
        newGraphNode g
        redrawGraph g

createNode :: Application Node
createNode g src = do
        trg <- newGraphNode g 
        createEdge g (src,trg)
        redrawGraph g 


nodename n = do
        nid <- getDaVinciObjectID n
        text ("Node[" ++ nid ++ "]") n

data NodeDescr =  
        NodeDescr {
                fName :: String, 
                fShape :: Shape, 
                fBorder :: Border
                }

editNode :: Application Node
editNode g n = do
        nd <- getNodeDescr n    
        iwin <- newNodeInputWin nd
        forkDialog iwin (\nd' -> do 
            incase nd' (setNodeDescr n)
            redrawGraph g)


newNodeInputWin :: NodeDescr -> IO (InputWin (Maybe NodeDescr))
newNodeInputWin nd = do 
        form <- newInputForm [flexible,value nd];
        newTextField [text "Name:",
                selector (lines . fName),
                modifier (\r t -> r{fName = unlines t}),
                parent form, 
                width 40,
                height 4
                ]
        newEnumField [Box .. Textual] [
                text "Shape:", 
                selector fShape,
                modifier (\r s -> r{fShape = s}), 
                parent form
                ]
        newEnumField [SingleBorder,DoubleBorder] [
                text "Border:", 
                selector fBorder,
                modifier (\r b -> r{fBorder = b}), 
                parent form
                ]
        newInputWin "Node Attributes" form Nothing [modal True]


getNodeDescr :: Node -> IO NodeDescr 
getNodeDescr n = do { 
        t <- getText n;
        s <- getShape n;
        b <- getBorder n;       
        return (NodeDescr t s b)
        }


setNodeDescr :: Node -> NodeDescr -> IO ()
setNodeDescr n (NodeDescr t s b) = do {
        configure n [text t, shape s, border b];
        done
        }
        

-- ---------------------------------------------------------------------------
-- Edge Applications
-- ---------------------------------------------------------------------------

createEdge :: Graph -> (Node,Node) -> IO ()
createEdge g (src,trg) = do 
        e <- newEdge Nothing src trg [edgetype (g,"EDGE")]
        ed <- getEdgeDefs
        setEdgeDescr e ed
        redrawGraph g 


deleteEdge :: Application Edge
deleteEdge g n = do {destroy n; redrawGraph g}

data EdgeDescr = EdgeDescr {fArrowHead :: ArrowHead, fPattern :: Pattern}

editEdge :: Application Edge
editEdge g e = do
        ed <- getEdgeDescr e    
        iwin <- newEdgeInputWin ed
        forkDialog iwin (\ed' -> do 
            incase ed' (setEdgeDescr e)
            redrawGraph g)

newEdgeInputWin :: EdgeDescr -> IO (InputWin (Maybe EdgeDescr))
newEdgeInputWin ed = do 
        form <- newInputForm [flexible,value ed];
        newEnumField [BothEnds .. NoHead] [
                text "Arrow Head:", 
                selector fArrowHead,
                modifier (\r s -> r{fArrowHead = s}), 
                parent form
                ]
        newEnumField [SolidLine .. DoubleLine] [
                text "Pattern:", 
                selector fPattern,
                modifier (\r b -> r{fPattern = b}), 
                parent form
                ]
        newInputWin "Edge Attributes" form Nothing [modal True]

getEdgeDescr :: Edge -> IO EdgeDescr 
getEdgeDescr e = do 
        h <- getArrowhead e
        p <- getPattern e
        return (EdgeDescr h p)

setEdgeDescr :: Edge -> EdgeDescr -> IO ()
setEdgeDescr e (EdgeDescr h' p') = do 
        configure e [arrowhead h', pattern p']
        done


-- ---------------------------------------------------------------------------
-- Iconbar
-- ---------------------------------------------------------------------------

newGraphIconBar :: Graph -> IO (IconBar ())
newGraphIconBar g = do 
        ib <- newIconBar []
        newButton [bmap' "ged_nodedel.xbm",withSel g deleteNode, parent ib]
        newButton [bmap' "ged_nodenew.xbm",withSel g createNode, parent ib]
        newButton [bmap' "ged_nodeattr.xbm",withSel g editNode, parent ib]
        newButton [bmap' "ged_edgedel.xbm",withSel g deleteEdge, parent ib]
        newButton [bmap' "ged_edgeattr.xbm",withSel g editEdge, parent ib]
        g # iconbar ib
        return ib

bmap' :: HasBitMap w => String -> Config w
bmap' fnm = bitmap ("/home/ewk/programs/workbench/davinci/test/" ++ fnm)

withSel :: HasSelectionBaseIndex Graph [a] =>
           Graph -> Application a -> Config (Button ()) 
withSel g f = command (\() -> do { 
   sel <- getSelection g;
   incase sel (mapM (f g))
   })

-- ---------------------------------------------------------------------------
-- Application Menu
-- ---------------------------------------------------------------------------

newGraphMenu :: Graph -> IO (Menu ())
newGraphMenu g = do
        mn <- newMenu []
        newButton [text "Default Node", reaction editNodeDefs, parent mn]
        newButton [text "Default Edge", reaction editEdgeDefs, parent mn]
        newButton [text "Exit", reaction logout, parent mn]
        g # menu mn
        return mn
         where logout = do {htk <- getToolInstance; destroy (htk ::HTk)}


editNodeDefs :: IO ()
editNodeDefs = do
        nd  <- getNodeDefs;
        iwin <- newNodeInputWin nd      
        forkDialog iwin (\nd' -> incase nd' (setNodeDefs))

editEdgeDefs :: IO ()
editEdgeDefs = do
        nd  <- getEdgeDefs;
        iwin <- newEdgeInputWin nd
        forkDialog iwin (\ed' -> incase ed' (setEdgeDefs))


-- ---------------------------------------------------------------------------
-- Default Node Attributes
-- ---------------------------------------------------------------------------

nodedefs :: MVar NodeDescr
nodedefs = IOExts.unsafePerformIO(newMVar (NodeDescr "" cdefault cdefault))

getNodeDefs :: IO NodeDescr
getNodeDefs = getVar nodedefs

setNodeDefs :: NodeDescr -> IO ()
setNodeDefs nd = setVar nodedefs nd     


-- ---------------------------------------------------------------------------
-- Default Edge Attributes
-- ---------------------------------------------------------------------------

edgedefs :: MVar EdgeDescr
edgedefs = IOExts.unsafePerformIO(newMVar (EdgeDescr cdefault cdefault))

getEdgeDefs :: IO EdgeDescr
getEdgeDefs = getVar edgedefs

setEdgeDefs :: EdgeDescr -> IO ()
setEdgeDefs nd = setVar edgedefs nd     

{- #########################################################################

MODULE        : DaVinciGraph
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {cpurper,ewk}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Management of DaVinci Graphs.


   ######################################################################### -}


module DaVinciGraph (
        Object(..),
        Synchronized(..),

        GraphOrientation(..),

        Graph,

        newGraph,

        lastGraphClosed,

        compact,

        edgedistance,
        getEdgeDistance,

        edgeradius,
        getEdgeRadius,

        fontsize,
        getFontSize,

        gapheight,
        getGapHeight,

        gapwidth,
        getGapWidth,

        statusmsg,
        getStatusMsg,

        showmsg,
        getShowMsg,

        graphorientation,
        getGraphOrientation,    

        keepNodesAtAllLevels,
        restoreAllSubGraphs,
        redrawGraph,

        improveAll,
        improveVisible,

        printGraph,
        saveGraph,
        saveGraphLayout,

        getNodes,
        getEdges,

        illegalEdgeGap,
        illegalEdgeRadius,
        illegalFontSize,
        illegalGapWidth,
        illegalGapHeight

        ) where

import SIM
import GUICore
import FiniteMap
import DaVinciCore
import DaVinciEvent
import DaVinciGraphTerm
import DaVinciClasses
import DaVinciMenu
import Char(isSpace)

import Debug(debug)



-- ---------------------------------------------------------------------------
--  Graph Creation
-- ---------------------------------------------------------------------------
 
newGraph :: [Config Graph] -> IO Graph 
newGraph opts = do
        grp <- createGraph
        configure grp graphdefaults
        configure grp opts
 where graphdefaults = [
                graphorientation TopDown,
                fontsize 12,
                gapwidth 20,
                gapheight 60,
                edgedistance 10,
                edgeradius 16   
                ]


-- ---------------------------------------------------------------------------
--  Instantiations
-- ---------------------------------------------------------------------------

instance GUIObject Graph where
        toGUIObject g = (fGraphObj g) 
        cname _ = "Graph"
        cset g cid val = do {
                setConfigValue (toGUIObject g) cid (toGUIValue val);
                return g
                }
        cget g cid =  do {
                val <- getConfigValue (toGUIObject g) cid ;
                case val of
                        Nothing -> return cdefault
                        (Just cv) -> return (fromGUIValue cv)
                }


instance Destructible Graph where
        destroy g = closeGraph g
        destroyed g = 
                ((listenDaVinci (g,Close)) :: IA DaVinciEventInfo) >>> do {
                        cleanupGraph g;
                        dav <- getToolInstance;
                        graphs <- getGraphs dav;
                        when (graphs == []) (
                                dispatch (fDispatcher dav) 
                                              (dav,LastGraphClosed)     
                                              NoDaVinciEventInfo
                                              done
                                )
                        }

instance DaVinciObject Graph where
        getGraphContext = return . id
        getDaVinciObjectID = return . show . objectID

instance HasFile Graph where
        filename fnm g =  
            withGraph g ( do {
                cset g "filename" fnm; 
                return ("menu(file(open_graph("++ show fnm ++")))")
                })
        getFileName g  = cget g "filename"


instance GUIValue v => HasText Graph v where
        text t g = withGraph g ( do {
                        cset g "title" t; 
                        return ("window(title("++show t++"))")
                        })
        getText g = cget g "title"


instance HasMenu Graph where
        menu mn g = do {installGraphMenu mn g; return g}        


-- ---------------------------------------------------------------------------
--  Events
-- ---------------------------------------------------------------------------

lastGraphClosed :: DaVinci -> IA ()
lastGraphClosed dav = listenDaVinci (dav,LastGraphClosed) >>> done

-- ---------------------------------------------------------------------------
--  Graph Specific Configure Options
-- ---------------------------------------------------------------------------

edgedistance :: Int -> Config Graph 
edgedistance s g = do {
        unless ((2<=s) && (s<=40)) (raise illegalEdgeGap);
        withGraph g (
                cset g "edgedistance" s >> 
                return ("set(multi_edge_gap("++show s++"))")
        )}

getEdgeDistance :: Graph -> IO Int
getEdgeDistance g = cget g "edgedistance"

edgeradius :: Int -> Config Graph 
edgeradius s g = do {
        unless ((8<=s) && (s<=60)) (raise illegalEdgeRadius);
        withGraph g (
                cset g "edgeradius" s >> 
                return ("set(self_edge_radius("++show s++"))")
        )}

getEdgeRadius :: Graph -> IO Int
getEdgeRadius g = cget g "edgeradius"

fontsize :: Int -> Config Graph 
fontsize s g = do {
        unless (elem s [6,8,10,12,14,18,24,36]) (raise illegalFontSize);
        withGraph g (
                cset g "fontsize" s >> 
                return ("set(font_size("++show s++"))")
        )} 

getFontSize :: Graph -> IO Int
getFontSize g = cget g "fontsize"

gapheight :: Int -> Config Graph
gapheight s g = do {
        unless ((4<=s) && (s<=300)) (raise illegalGapHeight);
        withGraph g (
                cset g "gapheight" s >> 
                return ("set(gap_height("++show s++"))")
        )}

getGapHeight :: Graph -> IO Int
getGapHeight g = cget g "gapheight"

gapwidth :: Int -> Config Graph 
gapwidth s g = do {
        unless ((4<=s) && (s<=300)) (raise illegalGapWidth) ;
        withGraph g (
                cset g "gapwidth" s >> 
                return ("set(gap_width("++show s++"))")
        )}

getGapWidth :: Graph -> IO Int 
getGapWidth g = cget g "gapwidth"

graphorientation :: GraphOrientation -> Config Graph 
graphorientation s g = withGraph g (
        cset g "orientation" s >> 
        return ("menu(layout(orientation("++show s++")))")
        )


getGraphOrientation :: Graph -> IO GraphOrientation 
getGraphOrientation g = cget g "orientation"

statusmsg :: String -> Config Graph                             
statusmsg t g = withGraph g (
        cset g "status" t >> 
        return ("window(show_status("++show t++"))")
        )

getStatusMsg :: Graph -> IO String
getStatusMsg g = cget g "status"

showmsg :: String -> Config Graph
showmsg t g = withGraph g (
        cset g "showmsg" t >> 
        return ("window(show_message("++show t++"))")
        )

getShowMsg :: Graph -> IO String
getShowMsg g = cget g "showmsg"


-- ---------------------------------------------------------------------------
-- Misc. Commands
-- ---------------------------------------------------------------------------

keepNodesAtAllLevels :: Bool -> Config Graph 
keepNodesAtAllLevels b g = 
        withGraph g (return "nothing")   {- TBD -} >>
        return g


restoreAllSubGraphs :: Graph -> IO () 
restoreAllSubGraphs g = do {
        redrawGraph g;
        withGraph g (return "menu(abstraction(restore_all_subgraphs))");
        done
        }

compact :: Toggle -> Config Graph
compact Off g = return g
compact On  g = withGraph g (return "menu(layout(compact_all))")


-- ---------------------------------------------------------------------------
-- Redisplay
-- ---------------------------------------------------------------------------

improveAll :: Graph -> IO ()                                    
improveAll g = do {
        withGraph g (return "menu(layout(improve_all))");
        done
        }

improveVisible :: Graph -> IO ()                                        
improveVisible g = do {
        withGraph g (return "menu(layout(improve_visible))");
        done
        }



-- ---------------------------------------------------------------------------
-- Files
-- ---------------------------------------------------------------------------

printGraph :: Maybe FilePath -> Graph -> IO ()
printGraph (Just fnm) g =  do {
        redrawGraph g;
        withGraph g (return ("menu(file(print("++show fnm ++")))"));
        done
        }
printGraph Nothing g =  do {
        redrawGraph g;
        withGraph g (return "menu(file(print))");
        done
        }


saveGraph :: FilePath -> Graph -> IO ()
saveGraph fnm g =  do {
        redrawGraph g;
        withGraph g (return ("menu(file(save_graph("++show fnm ++")))"));
        done
        }


saveGraphLayout :: FilePath -> Graph -> IO ()
saveGraphLayout fnm g = do { 
        redrawGraph g;
        withGraph g (return ("menu(file(save_status("++show fnm ++")))"));
        done
        }


-- ---------------------------------------------------------------------------
--  Other DaVinci Commands
-- ---------------------------------------------------------------------------

newFile :: IO ()                        
newFile = withDaVinci (return "menu(file(new))")

openGraphPlaced:: FilePath -> IO ()
openGraphPlaced file = 
        withDaVinci (return ("menu(file(open_graph_placed("++show file++")))"))

openStatus:: FilePath -> IO ()
openStatus file = 
        withDaVinci (return ("menu(file(open_status("++show file++")))"))


-- ---------------------------------------------------------------------------
-- GraphOrientation
-- ---------------------------------------------------------------------------

data GraphOrientation = TopDown | BottomUp | RightToLeft | LeftToRight

instance GUIValue GraphOrientation where
        cdefault = TopDown


instance Read GraphOrientation where
   readsPrec p b =
     case dropWhile (isSpace) b of
        't':'o':'p':'_':'d':'o':'w':'n':xs -> [(TopDown,xs)]
        'b':'o':'t':'t':'o':'m':'_':'u':'p':xs -> [(BottomUp,xs)]
        'r':'i':'g':'h':'t':'_':'l':'e':'f':'t':xs -> [(RightToLeft,xs)]
        'l':'e':'f':'t':'_':'r':'i':'g':'h':'t':xs -> [(LeftToRight,xs)]
        _ -> []

instance Show GraphOrientation where
   showsPrec d p r = 
      (case p of 
        TopDown -> "top_down" 
        BottomUp -> "bottom_up"
        RightToLeft -> "right_left"
        LeftToRight -> "left_right"
        ) ++ r

-- ---------------------------------------------------------------------------
-- IOErrors
-- ---------------------------------------------------------------------------

illegalEdgeGap :: IOError                       
illegalEdgeGap = userError "Multi edge gap out of range 2..40"          

illegalEdgeRadius :: IOError                    
illegalEdgeRadius = userError  "Self edge radius out of range 8..60" 

illegalFontSize :: IOError                      
illegalFontSize = userError "FontSize not in [6,8,10,12,14,18,24,36]"            
illegalGapWidth :: IOError                      
illegalGapWidth = userError "Gap width out of range 4..300"        

illegalGapHeight :: IOError                     
illegalGapHeight = userError "Gap height out of range 4..300" 


-- ---------------------------------------------------------------------------
-- Auxiliary
-- ---------------------------------------------------------------------------

dist2Int :: Distance -> Int 
dist2Int (Distance i) = i


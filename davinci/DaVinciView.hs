{- #########################################################################

MODULE        : DaVinciGraphView
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {cpurper,ewk}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : GraphViews.


CAVEATS       : The geometry of a window is the one defined by
                the application. There are no ways to get hold on
                the real geometry if the user moves or resizes the
                window!

                The view relevant commands operate on the entire
                view-pool associated with a graph, except those
                that operate on the socalled master-view. There is
                no way in which it is possible to control views
                one at a time (as in e.g. Tk/X-Windows).



   ######################################################################### -}


module DaVinciView (
        Graph,
        GraphView(..),

        displayGraph,
        fitScaleToWindow ,
        fullScale,
        getScaleFactor,
        scaleFactor,

        newDetailedView,
        newSurveyView,

        illegalDavCmd,
        illegalWindowSize,
        illegalScale    
        ) where

import SIM
import GUICore

import DaVinciCore 
import DaVinciGraph
import DaVinciGraphTerm
import DaVinciClasses


import Debug(debug)


-- ---------------------------------------------------------------------------
--  GraphView
-- ---------------------------------------------------------------------------

data GraphView = GraphView Graph deriving Eq

-- ---------------------------------------------------------------------------
--  Instances
-- ---------------------------------------------------------------------------

instance GUIObject GraphView where 
        toGUIObject (GraphView g) = toGUIObject g 
        cname _ = "GraphView"
        cset v @ (GraphView g) cid val = cset g cid val >> return v 
        cget (GraphView g) cid = cget g cid

instance Destructible GraphView where
        destroy _ = raise (illegalDavCmd  "GraphView.destroy")
        destroyed (GraphView g) = destroyed g
        

instance DaVinciObject GraphView where
        getGraphContext (GraphView g) = return g
        getDaVinciObjectID (GraphView g) = getDaVinciObjectID g


instance HasGeometry GraphView where
        geometry (w,h,x,y) view = synchronize view (do {
                position (x,y) view;
                size (w,h) view
                }) 
        getGeometry view = synchronize view (do {
                (x,y) <- getPosition view;
                (w,h) <- getSize view;
                return (w,h,x,y)
                })
                

instance HasSize GraphView where                                
        width w  view = synchronize view (do {
                (_,h) <- getSize view;
                size (w,h) view
                })
        getWidth view = getSize view >>= \ (w,_) -> return w
        height h   view = synchronize view (do {
                (w,_) <- getSize view;
                size (w,h) view
                })
        getHeight  view = getSize view >>= \ (_,h) -> return h
        size (w,h) v @ (GraphView g) = do {       
                unless ((0<w) && (0<h)) (raise illegalWindowSize);
                withGraph g (
                        cset g "size" (w,h) >>
                        return ("window(size("++show w ++","++ show h ++"))")
                        );
                return v
                }
        getSize view = cget view "size" 


instance HasPosition GraphView where                    
        position (x,y) v @ (GraphView g) = do {
                withGraph g (
                        cset g "position" (x,y) >> 
                        return ("window(position("++show x ++","++show y ++"))")
                        );
                return v
                }
        getPosition view = cget view "size"

instance Object GraphView where {
        objectID (GraphView g) = objectID g}


instance ToplevelWindow GraphView where
        iconify (GraphView g) = do {
                withGraph g (return "window(iconify)"); 
                done
                }
        deiconify (GraphView g) = do {
                withGraph g (return "window(deiconify)"); 
                done
                }
        withdraw view = raise (illegalDavCmd  "GraphView.withDraw")
        putWinOnTop (GraphView g) = do{
                withGraph g (return "window(raise)");
                done
                }
        putWinAtBottom view = raise (illegalDavCmd  "GraphView.putWinAtBottom")


instance Synchronized GraphView where
        synchronize w = synchronize (toGUIObject w)


-- ---------------------------------------------------------------------------
--  GraphView Creation
-- ---------------------------------------------------------------------------

displayGraph :: Graph -> IO GraphView 
displayGraph g = do {redrawGraph g; return (GraphView g)}       


newDetailedView :: Graph -> IO ()
newDetailedView g = do {
        withGraph g (return "menu(view(open_new_view))");
        done
        }


newSurveyView :: Graph -> IO ()
newSurveyView g = do {
        withGraph g (return "menu(view(open_survey_view))");
        done
        }


-- ---------------------------------------------------------------------------
--  GraphView Commands
-- ---------------------------------------------------------------------------

fitScaleToWindow :: Config GraphView
fitScaleToWindow v @ (GraphView g) = do {
        withGraph g (return "menu(view(fit_scale_to_window))");
        return v
        }

fullScale :: Config GraphView
fullScale v @ (GraphView g) = do {
        withGraph g (return "menu(view(full_scale))");
        return v
        }


getScaleFactor :: GraphView -> IO Int
getScaleFactor (GraphView g) = cget g "scale"

scaleFactor :: Int -> Config GraphView 
scaleFactor t v @ (GraphView g) = do { 
        unless ( (0 <= t) &&  (t <= 100) ) (raise illegalScale);
        withGraph g (
                cset g "scale" t >> 
                return ("menu(view(scale("++show t ++")))")
        );
        return v
        } 


-- ---------------------------------------------------------------------------
-- DaVinci Commands
-- ---------------------------------------------------------------------------

graphInfo :: Graph -> IO ()
graphInfo g = do {
        withGraph g (return "menu(view(graph_info))");
        done
        }       


-- ---------------------------------------------------------------------------
-- IOErrors
-- ---------------------------------------------------------------------------

illegalDavCmd :: String -> IOError
illegalDavCmd fnm = userError 
        (fnm ++ ": sorry, this operation is not supported yet by daVinci")

illegalWindowSize :: IOError                            
illegalWindowSize = userError "Window size must be greater than 0"
        
illegalScale :: IOError                 
illegalScale = userError "Scale out of range (0..100)"


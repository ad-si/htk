{- #########################################################################

MODULE        : DaVinci
AUTHOR        : Carla Blanck Purper
                Einar Karlsen,  
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The DaVinci Tool and its relevant commands.

TO BE DONE:     

        - destruction events

        - accelerators

        - file events



   ######################################################################### -}


module DaVinci (
        DaVinci,

        module DaVinciClasses,
        module DaVinciNodeType,
        module DaVinciEdgeType,
        module DaVinciRules,
        module DaVinciGraph,
        module DaVinciNode,
        module DaVinciEdge,
        module DaVinciView,
        module DaVinciDragAndDrop,
        module DaVinciSelection,
        module DaVinciIcon,
        module DaVinciPopupMenu,

        Set,
        Font,
        HasFont(..),
        FontDesignator(..),
        FontWeight,
        FontSlant,
        FontWidth,
 
        GUIOBJECT,
        GUIObject(..),
        GUIValue(..),

        Colour,
        HasColour(..),
        background,
        getBackground,
        foreground,
        getForeground,
        fg,
        bg,
 
        HasFile(..),

        Distance,
        HasSize(..),
        HasGeometry(..),
        HasPosition(..),
 
        HasText(..),

        Window,
        ToplevelWindow(..),

        Toggle,

        EventID,
        DaVinciEvent,
        EventDesignator(..),

        Tool(..),
        SingleInstanceTool(..),
        ToolStatus(..),

        davinci,

        getGraphs,
        closeGraph,
        delNode,
        delEdge,

        accuracy,
        getAccuracy,

        animationspeed,
        getAnimationSpeed,

        scrollingOnSelection,
        getScrollingOnSelection,

        cache,
        getCache,

        activate,
        deactivate,

        showUrl,

        illegalAnimationSpeed,
        illegalLayoutAccuracy                   
        ) where

import SIM
import GUICore
import DaVinciClasses
import DaVinciNodeType
import DaVinciEdgeType
import DaVinciRules
import DaVinciCore
import DaVinciGraphTerm
import DaVinciEvent
import DaVinciGraph
import DaVinciNode
import DaVinciEdge
import DaVinciView
import DaVinciSelection
import DaVinciDragAndDrop
import DaVinciIcon
import DaVinciPopupMenu

import BitMap
import Set
import Char(toLower)


import Debug(debug)




-- ---------------------------------------------------------------------------
--  Startup daVinci
-- ---------------------------------------------------------------------------

davinci :: [Config DaVinci] -> IO DaVinci
davinci opts = do {
        dav <- getToolInstance;
        configure dav graphtooldefaults ;
        configure dav opts;
        registerTool dav;
        return dav
} where graphtooldefaults = [
                animationspeed 25,
--              cache True,
                accuracy 3
                ]


-- ---------------------------------------------------------------------------
--  Instances
-- ---------------------------------------------------------------------------

instance GUIObject DaVinci  where
         toGUIObject dav = fSession dav
         cname _ = "DaVinci"
         cset dav cid val = 
                setConfigValue (toGUIObject dav) cid (toGUIValue val) >> return dav
         cget dav cid = do {
                x <- getConfigValue (toGUIObject dav) cid ;
                case x of
                        Nothing -> return cdefault
                        (Just cv) -> return (fromGUIValue cv)
                }

instance Synchronized DaVinci where
        synchronize w = synchronize (toGUIObject w)

-- ---------------------------------------------------------------------------
--  Commands
-- ---------------------------------------------------------------------------

accuracy :: Int -> Config DaVinci 
accuracy a g = 
        unless ((1<=a)&& (a<=5)) (raise illegalLayoutAccuracy) >>
        withDaVinci (
                cset g "accuracy" a >> 
                return ("set(layout_accuracy("++show a++"))")
        ) >> return g

getAccuracy :: DaVinci -> IO Int
getAccuracy g = cget g "accuracy"

animationspeed :: Int -> Config DaVinci
animationspeed s g =
        unless ((s==0) || ((10<=s) && (s<=100))) (raise illegalAnimationSpeed)>>
        withDaVinci (
                cset g "animationspeed" s                       >> 
                return ("set(animation_speed("++show s++"))")
        ) >> return g

getAnimationSpeed :: DaVinci -> IO Int
getAnimationSpeed g = cget g "animationspeed"


scrollingOnSelection :: Toggle -> Config DaVinci
scrollingOnSelection s g =
        withDaVinci (
                cset g "scrollingOnSelection" s                         >> 
                return ("set(scrolling_on_selection("++ toDaVinciBool s ++"))")
        ) >> return g

getScrollingOnSelection :: DaVinci -> IO Toggle
getScrollingOnSelection g = 
        cget g "scrollingOnSelection" >>=  return . fromDaVinciBool

cache :: Toggle -> Config DaVinci
cache b g =  
        withDaVinci (
                cset g "cache" b                                        >> 
                return ("set(no_cache("++ toDaVinciBool b ++"))")
        ) >> return g

getCache :: DaVinci -> IO Toggle
getCache g = cget g "cache" >>= return . fromDaVinciBool

activate :: Config DaVinci
activate dav = withDaVinci (return "window(activate)") >> return dav

deactivate :: Config DaVinci
deactivate dav = withDaVinci ( return "window(deactivate)") >> return dav
 



-- ---------------------------------------------------------------------------
--  Url's
-- ---------------------------------------------------------------------------

showUrl :: String -> IO ()
showUrl url = do {
        withDaVinci (return ("special(show_url("++show url++"))"));
        done
        }


-- ---------------------------------------------------------------------------
-- IOErrors
-- ---------------------------------------------------------------------------

illegalAnimationSpeed :: IOError                        
illegalAnimationSpeed = userError "Animation speed out of range 0, 10..100"

illegalLayoutAccuracy :: IOError                        
illegalLayoutAccuracy = userError "Layout Accuracy out of range (1..5)"  



-- ---------------------------------------------------------------------------
-- DaVinci Bool
-- ---------------------------------------------------------------------------

toDaVinciBool On = "true"
toDaVinciBool Off = "false"

fromDaVinciBool "true" = On
fromDaVinciBool "false" = Off

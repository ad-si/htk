{- #########################################################################

MODULE        : Canvas
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha


   ######################################################################### -}


module Canvas (
        Distance,
        Position,
        Geometry,

        HasPostscript(..),

        ScrollBar,
        HasScroller(..),
        ScrollUnit,

        Canvas,
        newCanvas,

        closeEnough,
        getCloseEnough,
        
        confine,
        getConfine,

        screenToCanvasCoord,

        ScrollRegion,
        scrollRegion,
        getScrollRegion,

        scrollIncrement,
        getScrollIncrement

        ) where

import Concurrency
import GUICore
import Image
import ScrollBar
import Printer
import Debug(debug)


-- --------------------------------------------------------------------------
-- Canvas
-- --------------------------------------------------------------------------

newtype Canvas = Canvas GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newCanvas :: [Config Canvas] -> IO Canvas
newCanvas ol = do
        w <- createGUIObject CANVAS canvasMethods
        configure (Canvas w) ol


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance GUIObject Canvas where
        toGUIObject (Canvas w) = w 
        cname _ = "Canvas"

instance Destructible Canvas where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Canvas

instance Widget Canvas

instance ChildWidget Canvas

instance Synchronized Canvas where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder Canvas

instance HasColour Canvas where 
        legalColourID = hasBackGroundColour

instance HasEnable Canvas

instance HasSize Canvas

instance HasScroller Canvas

instance HasPostscript Canvas


-- --------------------------------------------------------------------------
-- Configuration Options
-- --------------------------------------------------------------------------

closeEnough :: Double -> Config Canvas
closeEnough d w = cset w "closeenough" d

getCloseEnough :: Canvas -> IO Double
getCloseEnough w = cget w "closeenough"

confine :: Bool -> Config Canvas
confine d w = cset w "confine" d

getConfine :: Canvas -> IO Bool
getConfine w = cget w "confine"

-- --------------------------------------------------------------------------
-- Coordinates
-- --------------------------------------------------------------------------

screenToCanvasCoord :: Canvas -> Orientation -> Distance -> Maybe Distance -> IO Distance
screenToCanvasCoord cv ax d sp = evalMethod cv (\nm -> tkCanvas nm ax d sp)

-- --------------------------------------------------------------------------
-- Scrolling
-- --------------------------------------------------------------------------

type ScrollRegion = (Position,Position,Position,Position)

scrollRegion :: ScrollRegion -> Config Canvas
scrollRegion (p1,p2,p3,p4) w = cset w "scrollregion" [p1,p2,p3,p4]


getScrollRegion :: Canvas -> IO ScrollRegion
getScrollRegion can = 
   cget can "scrollregion" >>= \[p1,p2,p3,p4] -> return (p1,p2,p3,p4)


scrollIncrement :: Orientation -> Distance -> Config Canvas
scrollIncrement Horizontal d w = cset w  "xscrollincrement" d
scrollIncrement Vertical d w   = cset w "yscrollincrement" d


getScrollIncrement :: Orientation -> Canvas -> IO Distance
getScrollIncrement Horizontal can = cget can  "xscrollincrement"
getScrollIncrement Vertical can = cget can "yscrollincrement"


-- --------------------------------------------------------------------------
-- Canvas Methods
-- --------------------------------------------------------------------------

canvasMethods = defMethods {
                cleanupCmd = tkCleanupCanvas,
                createCmd = tkCreateCanvas
                }


-- --------------------------------------------------------------------------
-- Tk Commands
-- --------------------------------------------------------------------------

tkCreateCanvas :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> 
                        TclScript
tkCreateCanvas kind name oid confs = 
        tkDeclVar ("sv" ++ show oid) (show name) ++ 
        (createCmd defMethods) kind name oid confs 
{-# INLINE tkCreateCanvas #-}


tkCleanupCanvas :: ObjectID -> ObjectName -> TclScript
tkCleanupCanvas oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupCanvas #-}


tkCanvas :: ObjectName -> Orientation -> Distance -> Maybe Distance -> TclScript
tkCanvas nm Horizontal d sp = [show nm ++ " canvasx " ++ show d ++ showGrid sp]
tkCanvas nm Vertical d sp = [show nm ++ " canvasy " ++ show d ++ showGrid sp]
{-# INLINE tkCanvas #-}

showGrid Nothing = ""
showGrid (Just gs) = " " ++ show gs

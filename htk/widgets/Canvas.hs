-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

--- 
-- HTk's <strong>canvas widget</strong>.<br>
-- A canvas is a drawing pad, that can also contain widgets in embedded
-- windows.<br>
-- A canvas widget contains <strong>canvas items</strong>.
module Canvas (

--  HasPostscript(..),   -- TD: ps export

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
--  getScrollIncrementer

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Geometry
import Image
import ScrollBar
--import Printer        -- TD: PS export
import Computation
import Destructible
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- canvas
-- -----------------------------------------------------------------------

---
-- The <code>Canvas</code> datatype.
newtype Canvas = Canvas GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new canvas widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this canvas.
-- @return result - A canvas widget.
newCanvas :: Container par => par -> [Config Canvas] -> IO Canvas
newCanvas par cnf = do
  w <- createGUIObject (toGUIObject par) CANVAS canvasMethods
  configure (Canvas w) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Canvas where
---
-- Internal.
  toGUIObject (Canvas w) = w 
---
-- Internal.
  cname _ = "Canvas"

---
-- A canvas widget can be destroyed.
instance Destroyable Canvas where
---
-- Destroys a canvas widget.
  destroy   = destroy . toGUIObject

---
-- A canvas widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Canvas

---
-- A canvas is also a container for widgets, because it can contain
-- widgets in embedded windows.
instance Container Canvas

---
-- A canvas widget has a configureable border.
instance HasBorder Canvas

---
-- A canvas widget has a foreground and background colour.
instance HasColour Canvas where 
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- A canvas widget is a stateful widget, it can be enabled or disabled.
instance HasEnable Canvas

---
-- You can specify the size of a canvas.
instance HasSize Canvas

---
-- A canvas is a scrollable widget.
instance HasScroller Canvas

--instance HasPostscript Canvas

---
-- You can synchronize on a canvas object (in JAVA style).
instance Synchronized Canvas where
---
-- Synchronizes on a canvas object.
  synchronize = synchronize . toGUIObject

---
-- A canvas can have a tooltip (only displayed if you are using tixwish).
instance HasTooltip Canvas


-- -----------------------------------------------------------------------
-- canvas-specific configuration options
-- -----------------------------------------------------------------------

---
-- Sets the maximum distance from the mouse to an overlapped object.
-- @param dist    - the distance to be set.
-- @param cnv     - the canvas to apply this configuration.
-- @return result - The concerned canvas.
closeEnough :: Double -> Canvas -> IO Canvas
closeEnough dist cnv = cset cnv "closeenough" dist

---
-- Selector for the maximum distance from the mouse to an overlapped
-- object.
-- @param cnv     - the canvas to get this configuration from.
-- @return result - The requested distance.
getCloseEnough :: Canvas -> IO Double
getCloseEnough cnv = cget cnv "closeenough"

---
-- <code>True</code> constraints view to the scroll region.
-- @param b       - <code>Bool</code>, see above.
-- @param cnv     - the canvas to apply this configuration.
-- @return result - The concerned canvas.
confine :: Bool -> Canvas -> IO Canvas
confine b cnv = cset cnv "confine" b

---
-- Selector for the <code>confine</code> configuration, constraints view
-- to the scroll region if <code>True</code>.
-- @param cnv     - the canvas to get this configuration from.
-- @return result - The confine configuration as a <code>Bool</code>
--                  value (see <code>confine</code>).
getConfine :: Canvas -> IO Bool
getConfine w = cget w "confine"


-- -----------------------------------------------------------------------
-- coordinate transformation
-- -----------------------------------------------------------------------

---
-- Maps from screen X or Y coordinates (orientation parameter) to the
-- corresponding coordinates in canvas space.
-- @param cnv     - the concerned canvas widget.
-- @param orient  - the orientation
--                  (<code>Vertical</code> or <code>Horizontal</code>).
-- @param dist    - the input coordinate.
-- @param grid    - an optional grid (the output can be rounded to
--                  multiples of this grid if specified).
-- @return result - The requested distance in the specified orientation.
screenToCanvasCoord :: Canvas -> Orientation -> Distance ->
                       Maybe Distance -> IO Distance
screenToCanvasCoord cnv orient dist grid =
  evalMethod cnv (\nm -> tkCanvas nm orient dist grid)


-- -----------------------------------------------------------------------
-- scrolling
-- -----------------------------------------------------------------------

---
-- The <code>ScrollRegion</code> datatype (scrollable region of the canvas
-- widget).
type ScrollRegion = (Position, Position)

---
-- Sets the scrollable region for a canvas widget.
-- @param reg     - the scroll region to set.
-- @param cnv     - the canvas widget to apply this scrollregion.
-- @return result - The concerned canvas.
scrollRegion :: ScrollRegion -> Canvas -> IO Canvas
scrollRegion reg@((x1, y1), (x2, y2)) cnv =
  let reg = " { " ++ show x1 ++ " " ++ show y1 ++ " " ++ show x2 ++ " " ++
            show y2 ++ " }"
  in cset cnv ("scrollregion" ++ reg) ([] :: [Position])

---
-- Gets the applied scroll region from a canvas widget.
-- @param cnv     - the canvas widget to get the applied scroll region
--                  from.
-- @return result - The requested scroll region.
getScrollRegion :: Canvas -> IO ScrollRegion
getScrollRegion cnv =
   cget cnv "scrollregion" >>= \reg ->
                                  case reg of
                                    [p1,p2] -> return (p1,p2)
                                    _ -> return ((0,0), (0,0))

---
-- Sets the distance for one scrolling unit.
-- @param orient  - the orientation 
--                  (<code>Vertical</code> or <code>Horizontal</code>).
-- @param dist    - the distance to set.
-- @param cnv     - the canvas widget to apply this scrolling
--                  distance.
-- @return result - The concerned canvas.
scrollIncrement :: Orientation -> Distance -> Canvas -> IO Canvas
scrollIncrement orient dist cnv =
  case orient of Horizontal -> cset cnv  "xscrollincrement" dist
                 _ -> cset cnv "yscrollincrement" dist

---
-- Gets the applied minimum scrolling distance from a canvas widget.
-- @param orient  - the orientation
--                  (<code>Vertical</code> or <code>Horizontal</code>).
-- @param cnv     - the canvas widget to get the applied minimum
--                  scrolling distance from.
-- @return result - The requested minimum scrolling distance.
getScrollIncrement :: Orientation -> Canvas -> IO Distance
getScrollIncrement orient cnv =
  case orient of Horizontal -> cget cnv "xscrollincrement"
                 Vertical -> cget cnv "yscrollincrement"


-- -----------------------------------------------------------------------
-- canvas methods
-- -----------------------------------------------------------------------

canvasMethods = defMethods { cleanupCmd = tkCleanupCanvas,
                             createCmd = tkCreateCanvas }


-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkCreateCanvas :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                  [ConfigOption] -> TclScript
tkCreateCanvas pnm kind name oid confs = 
  tkDeclVar ("sv" ++ show oid) (show name) ++ 
  (createCmd defMethods) pnm kind name oid confs 
{-# INLINE tkCreateCanvas #-}

tkCleanupCanvas :: ObjectID -> ObjectName -> TclScript
tkCleanupCanvas oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupCanvas #-}

tkCanvas :: ObjectName -> Orientation -> Distance -> Maybe Distance ->
            TclScript
tkCanvas nm Horizontal d sp =
  [show nm ++ " canvasx " ++ show d ++ showGrid sp]
tkCanvas nm Vertical d sp =
  [show nm ++ " canvasy " ++ show d ++ showGrid sp]
{-# INLINE tkCanvas #-}

showGrid Nothing = ""
showGrid (Just gs) = " " ++ show gs

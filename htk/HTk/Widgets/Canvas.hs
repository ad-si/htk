{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTk's <strong>canvas widget</strong>.<br>
-- A canvas is a drawing pad, that can also contain widgets in embedded
-- windows.<br>
-- A canvas widget contains <strong>canvas items</strong>.
module HTk.Widgets.Canvas (

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

import Control.Exception

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Geometry
import HTk.Widgets.ScrollBar
import HTk.Devices.Printer
import Util.Computation
import Events.Destructible
import Events.Synchronized
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- canvas
-- -----------------------------------------------------------------------

-- | The @Canvas@ datatype.
newtype Canvas = Canvas GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new canvas widget and returns a handler.
newCanvas :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Canvas]
   -- ^ the list of configuration options for this canvas.
   -> IO Canvas
   -- ^ A canvas widget.
newCanvas par cnf = do
  w <- createGUIObject (toGUIObject par) CANVAS canvasMethods
  configure (Canvas w) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Canvas where
  toGUIObject (Canvas w) = w
  cname _ = "Canvas"

-- | A canvas widget can be destroyed.
instance Destroyable Canvas where
  --  Destroys a canvas widget.
  destroy   = destroy . toGUIObject

-- | A canvas widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Canvas

-- | A canvas is also a container for widgets, because it can contain
-- widgets in embedded windows.
instance Container Canvas

-- | A canvas widget has a configureable border.
instance HasBorder Canvas

-- | A canvas widget has a foreground and background colour.
instance HasColour Canvas where
  legalColourID = hasBackGroundColour

-- | A canvas widget is a stateful widget, it can be enabled or disabled.
instance HasEnable Canvas

-- | You can specify the size of a canvas.
instance HasSize Canvas

-- | A canvas is a scrollable widget.
instance HasScroller Canvas

-- | The contents of a canvas is printable.
instance HasPostscript Canvas

-- | You can synchronize on a canvas object (in JAVA style).
instance Synchronized Canvas where
  --  Synchronizes on a canvas object.
  synchronize = synchronize . toGUIObject

-- | A canvas can have a tooltip (only displayed if you are using tixwish).
instance HasTooltip Canvas


-- -----------------------------------------------------------------------
-- canvas-specific configuration options
-- -----------------------------------------------------------------------

-- | Sets the maximum distance from the mouse to an overlapped object.
closeEnough :: Double
   -- ^ the distance to be set.
   -> Canvas
   -- ^ the canvas to apply this configuration.
   -> IO Canvas
   -- ^ The concerned canvas.
closeEnough dist cnv = cset cnv "closeenough" dist

-- | Selector for the maximum distance from the mouse to an overlapped
-- object.
getCloseEnough :: Canvas
   -- ^ the canvas to get this configuration from.
   -> IO Double
   -- ^ The requested distance.
getCloseEnough cnv = cget cnv "closeenough"

-- | @True@ constraints view to the scroll region.
confine :: Bool
   -- ^ @Bool@, see above.
   -> Canvas
   -- ^ the canvas to apply this configuration.
   -> IO Canvas
   -- ^ The concerned canvas.
confine b cnv = cset cnv "confine" b

-- | Selector for the @confine@ configuration, constraints view
-- to the scroll region if @True@.
getConfine :: Canvas
   -- ^ the canvas to get this configuration from.
   -> IO Bool
   -- ^ The confine configuration as a @Bool@
   -- value (see @confine@).
getConfine w = cget w "confine"


-- -----------------------------------------------------------------------
-- bounding boxes
-- -----------------------------------------------------------------------

-- | You can request the bounding box size of a canvas item (use a canvas
-- tag for the bounding box of a set of items).
instance GUIObject c => HasBBox Canvas c where

-- Gets the bounding box of a canvas item.
--     cnv     - the concerned canvas.
--     item    - the concerned canvas item.
-- result is the requested bounding box (upper left position,
-- lower right position).
  bbox cnv item =
    do
      objnm <- getObjectName (toGUIObject item)
      ans <- try (evalMethod cnv (\nm -> tkBBox nm objnm))
      case ans of
        Left (e :: SomeException) -> return Nothing
        Right a -> return (Just a)

tkBBox :: ObjectName -> ObjectName -> TclScript
tkBBox nm (CanvasItemName _ cid) =
  ["global " ++ drop 1 (show cid), show nm ++ " bbox " ++ show cid]
tkBBox _ _ = []
{-# INLINE tkBBox #-}


-- -----------------------------------------------------------------------
-- coordinate transformation
-- -----------------------------------------------------------------------

-- | Maps from screen X or Y coordinates (orientation parameter) to the
-- corresponding coordinates in canvas space.
screenToCanvasCoord :: Canvas
   -- ^ the concerned canvas widget.
   -> Orientation
   -- ^ the orientation
   -- (@Vertical@ or @Horizontal@).
   -> Distance
   -- ^ the input coordinate.
   ->
   Maybe Distance
   -- ^ an optional grid (the output can be rounded to
   -- multiples of this grid if specified).
   -> IO Distance
   -- ^ The requested distance in the specified orientation.
screenToCanvasCoord cnv orient dist grid =
  evalMethod cnv (\nm -> tkCanvas nm orient dist grid)


-- -----------------------------------------------------------------------
-- scrolling
-- -----------------------------------------------------------------------

-- | The @ScrollRegion@ datatype (scrollable region of the canvas
-- widget).
type ScrollRegion = (Position, Position)

-- | Sets the scrollable region for a canvas widget.
scrollRegion :: ScrollRegion
   -- ^ the scroll region to set.
   -> Canvas
   -- ^ the canvas widget to apply this scrollregion.
   -> IO Canvas
   -- ^ The concerned canvas.
scrollRegion reg@((x1, y1), (x2, y2)) cnv =
  let reg = " { " ++ show x1 ++ " " ++ show y1 ++ " " ++ show x2 ++ " " ++
            show y2 ++ " }"
  in cset cnv ("scrollregion" ++ reg) ([] :: [Position])

-- | Gets the applied scroll region from a canvas widget.
getScrollRegion :: Canvas
   -- ^ the canvas widget to get the applied scroll region
   -- from.
   -> IO ScrollRegion
   -- ^ The requested scroll region.
getScrollRegion cnv =
   cget cnv "scrollregion" >>= \reg ->
                                  case reg of
                                    [p1,p2] -> return (p1,p2)
                                    _ -> return ((0,0), (0,0))

-- | Sets the distance for one scrolling unit.
scrollIncrement :: Orientation
   -- ^ the orientation
   -- (@Vertical@ or @Horizontal@).
   -> Distance
   -- ^ the distance to set.
   -> Canvas
   -- ^ the canvas widget to apply this scrolling
   -- distance.
   -> IO Canvas
   -- ^ The concerned canvas.
scrollIncrement orient dist cnv =
  case orient of Horizontal -> cset cnv  "xscrollincrement" dist
                 _ -> cset cnv "yscrollincrement" dist

-- | Gets the applied minimum scrolling distance from a canvas widget.
getScrollIncrement :: Orientation
   -- ^ the orientation
   -- (@Vertical@ or @Horizontal@).
   -> Canvas
   -- ^ the canvas widget to get the applied minimum
   -- scrolling distance from.
   -> IO Distance
   -- ^ The requested minimum scrolling distance.
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

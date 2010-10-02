-- | HTk\'s /bitmap/ canvas item.
-- A bitmap object on a canvas widget.
module HTk.Canvasitems.BitMapItem (

  BitMapItem,
  createBitMapItem

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Kernel.Colour(toColour)
import HTk.Canvasitems.CanvasItem
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.CanvasItemAux
import HTk.Components.BitMap
import Util.Computation
import Events.Synchronized
import Events.Destructible


-- -----------------------------------------------------------------------
-- BitMapItem
-- -----------------------------------------------------------------------

-- | The @BitMapItem@ datatype.
newtype BitMapItem = BitMapItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new bitmap item.
createBitMapItem :: Canvas
   -- ^ the parent canvas.
   -> [Config BitMapItem]
   -- ^ the list of configuration options for this bitmap
   -- item.
   -> IO BitMapItem
   -- ^ A bitmap item.
createBitMapItem cnv cnf =
  createCanvasItem cnv BITMAPITEM BitMapItem cnf [(-1,-1)]


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject BitMapItem where
  toGUIObject (BitMapItem w) = w
  cname _ = "BitMapItem"

-- | A bitmap item can be destroyed.
instance Destroyable BitMapItem where
  -- Destroys a bitmap item.
  destroy = destroy . toGUIObject

-- | A bitmap item is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem BitMapItem

-- | A bitmap item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem BitMapItem

-- | You can specify the position of a bitmap item.
instance HasPosition BitMapItem where
  -- Sets the position of the bitmap item.
  position        = itemPositionD2
  -- Gets the position of the bitmap item.
  getPosition     = getItemPositionD2

-- | You can specify the anchor position of a bitmap item.
instance HasCanvAnchor BitMapItem where
  -- Sets the anchor position of a bitmap item.
  canvAnchor a w = cset w "anchor" a
  -- Gets the anchor position of a bitmap item.
  getCanvAnchor w = cget w "anchor"

-- | A bitmap item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem BitMapItem where
  -- Sets the filling (foreground) of a bitmap item.
  filling c w       = cset w "foreground" (toColour c)
  -- Gets the filling (foreground) of a bitmap item.
  getFilling w      = cget w "foreground"
  -- Sets the outline (background) of a bitmap item.
  outline c w       = cset w "background" (toColour c)
  -- Gets the outline (background) of a bitmap item.
  getOutline w      = cget w "background"
  -- Dummy configuration (no effect).
  outlinewidth c w  = return w
  -- Dummy configuration (no effect).
  getOutlineWidth w = return cdefault
  -- Sets the bitmap handle for this item.
  stipple b w       = setBitMapHandle w "bitmap" b True
  -- Gets the bitmap handle for this item.
  getStipple w      = getBitMapHandle w "bitmap"

-- | A bitmap item is a container for a bitmap object.
instance HasBitMap BitMapItem

-- | You can synchronize on a bitmap item.
instance Synchronized BitMapItem where
  -- Synchronizes on a bitmap item.
  synchronize w = synchronize (toGUIObject w)

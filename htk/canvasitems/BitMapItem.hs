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
-- HTk's <strong>bitmap</strong> canvas item.<br>
-- A bitmap object on a canvas widget.
module BitMapItem (

  module CanvasItem,

  BitMapItem,
  createBitMapItem

) where

import Core
import Configuration
import Colour(toColour)
import CanvasItem
import CanvasTag
import CanvasItemAux
import BitMap
import Computation
import Synchronized
import Destructible


-- -----------------------------------------------------------------------
-- BitMapItem
-- -----------------------------------------------------------------------

---
-- The <code>BitMapItem</code> datatype.
newtype BitMapItem = BitMapItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new bitmap item.
-- @param cnv     - the parent canvas.
-- @param cnf     - the list of configuration options for this bitmap
--                  item.
-- @return result - A bitmap item.
createBitMapItem :: Canvas -> [Config BitMapItem] -> IO BitMapItem
createBitMapItem cnv cnf =
  createCanvasItem cnv BITMAPITEM BitMapItem cnf [(-1,-1)]


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject BitMapItem where 
---
-- Internal.
  toGUIObject (BitMapItem w) = w
---
-- Internal.
  cname _ = "BitMapItem"

---
-- A bitmap item can be destroyed.
instance Destroyable BitMapItem where
---
-- Destroys a bitmap item.
  destroy = destroy . toGUIObject

---
-- A bitmap item is a canvas item (any canvas item is an instance of the
-- abstract <code>class CanvasItem</code>).
instance CanvasItem BitMapItem

---
-- A bitmap item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem BitMapItem

---
-- You can specify the position of a bitmap item.
instance HasPosition BitMapItem where
---
-- Sets the position of the bitmap item.
  position        = itemPositionD2
---
-- Gets the position of the bitmap item.
  getPosition     = getItemPositionD2

---
-- You can specify the anchor position of a bitmap item.
instance HasCanvAnchor BitMapItem where
---
-- Sets the anchor position of a bitmap item.
  canvAnchor a w = cset w "anchor" a
---
-- Gets the anchor position of a bitmap item.
  getCanvAnchor w = cget w "anchor"

---
-- A bitmap item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem BitMapItem where
---
-- Sets the filling (foreground) of a bitmap item.
  filling c w       = cset w "foreground" (toColour c)
---
-- Gets the filling (foreground) of a bitmap item.
  getFilling w      = cget w "foreground"
---
-- Sets the outline (background) of a bitmap item.
  outline c w       = cset w "background" (toColour c)
---
-- Gets the outline (background) of a bitmap item.
  getOutline w      = cget w "background"
---
-- Dummy configuration (no effect).
  outlinewidth c w  = return w
---
-- Dummy configuration (no effect).
  getOutlineWidth w = return cdefault
---
-- Sets the bitmap handle for this item.
  stipple b w       = setBitMapHandle w "bitmap" b True
---
-- Gets the bitmap handle for this item.
  getStipple w      = getBitMapHandle w "bitmap"

---
-- A bitmap item is a container for a bitmap object.
instance HasBitMap BitMapItem

---
-- You can synchronize on a bitmap item.
instance Synchronized BitMapItem where
---
-- Synchronizes on a bitmap item.
  synchronize w = synchronize (toGUIObject w)

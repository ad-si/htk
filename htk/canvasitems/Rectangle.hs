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
-- HTk's <strong>rectangle</strong> canvas item.<br>
-- A rectangle object on a canvas widget.
module Rectangle (

  module CanvasItem,

  Rectangle,
  createRectangle

) where

import Core
import Configuration
import CanvasItem
import CanvasTag
import CanvasItemAux
import Destructible
import Computation
import Synchronized


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>Rectangle</code> datatype.
newtype Rectangle = Rectangle GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

---
-- Constructs a new rectangle item.
-- @param cnv     - the parent canvas.
-- @param cnf     - the list of configuration options for this rectangle
--                  item.
-- @return result - A rectangle item.
createRectangle :: Canvas -> [Config Rectangle] -> IO Rectangle
createRectangle cnv cnf =
  createCanvasItem cnv RECTANGLE Rectangle cnf [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Rectangle where 
---
-- Internal.
  toGUIObject (Rectangle w) = w
---
-- Internal.
  cname _ = "Rectangle"

---
-- A rectangle item can be destroyed.
instance Destroyable Rectangle where
---
-- Destroys a rectangle item.
  destroy = destroy . toGUIObject

---
-- You can synchronize on a rectangle item.
instance Synchronized Rectangle where
---
-- Synchronize on a rectangle item.
  synchronize = synchronize . toGUIObject

---
-- A rectangle item is a canvas item (any canvas item is an instance of
-- the abstract <code>class CanvasItem</code>).
instance CanvasItem Rectangle

---
-- A rectangle item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem Rectangle

---
-- A rectangle item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem Rectangle

---
-- An alternative way to specify a rectangle's coords.
instance HasGeometry Rectangle where
---
-- Sets the geometry of a rectangle (width, height, upper left position).
  geometry = itemGeo
---
-- Gets the geometry of a rectangle (width, height, upper left position).
  getGeometry = getGeo

---
-- You can specify the (upper left) position of a rectangle.
instance HasPosition Rectangle where
---
-- Sets the (upper left) position of a rectangle.
  position = itemPosition
---
-- Gets the (upper left) position of a rectangle.
  getPosition = getItemPosition

---
-- You can specify the size of an rectangle item.
instance HasSize Rectangle where
---
-- Sets the width of a rectangle item.
  width = itemWidth
---
-- Gets the width of a rectangle item.
  getWidth = getItemWidth
---
-- Sets the height of a rectangle item.
  height = itemHeight
---
-- Gets the height of a rectangle item.
  getHeight = getItemHeight
---
-- Sets the size (width, height) of a rectangle item.
  size = itemSize
---
-- Gets the size (width, height) of a rectangle item.
  getSize = getItemSize

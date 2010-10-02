-- | HTk\'s /rectangle/ canvas item.
-- A rectangle object on a canvas widget.
module HTk.Canvasitems.Rectangle (

  Rectangle,
  createRectangle

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Canvasitems.CanvasItem
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.CanvasItemAux
import Events.Destructible
import Util.Computation
import Events.Synchronized


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Rectangle@ datatype.
newtype Rectangle = Rectangle GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Constructs a new rectangle item.
createRectangle :: Canvas
   -- ^ the parent canvas.
   -> [Config Rectangle]
   -- ^ the list of configuration options for this rectangle
   -- item.
   -> IO Rectangle
   -- ^ A rectangle item.
createRectangle cnv cnf =
  createCanvasItem cnv RECTANGLE Rectangle cnf [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Rectangle where
  toGUIObject (Rectangle w) = w
  cname _ = "Rectangle"

-- | A rectangle item can be destroyed.
instance Destroyable Rectangle where
  -- Destroys a rectangle item.
  destroy = destroy . toGUIObject

-- | You can synchronize on a rectangle item.
instance Synchronized Rectangle where
  -- Synchronize on a rectangle item.
  synchronize = synchronize . toGUIObject

-- | A rectangle item is a canvas item (any canvas item is an instance of
-- the abstract @class CanvasItem@).
instance CanvasItem Rectangle

-- | A rectangle item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem Rectangle

-- | A rectangle item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem Rectangle

-- | An alternative way to specify a rectangle\'s coords.
instance HasGeometry Rectangle where
  -- Sets the geometry of a rectangle (width, height, upper left position).
  geometry = itemGeo
  -- Gets the geometry of a rectangle (width, height, upper left position).
  getGeometry = getGeo

-- | You can specify the (upper left) position of a rectangle.
instance HasPosition Rectangle where
  -- Sets the (upper left) position of a rectangle.
  position = itemPosition
  -- Gets the (upper left) position of a rectangle.
  getPosition = getItemPosition

-- | You can specify the size of an rectangle item.
instance HasSize Rectangle where
  -- Sets the width of a rectangle item.
  width = itemWidth
  -- Gets the width of a rectangle item.
  getWidth = getItemWidth
  -- Sets the height of a rectangle item.
  height = itemHeight
  -- Gets the height of a rectangle item.
  getHeight = getItemHeight
  -- Sets the size (width, height) of a rectangle item.
  size = itemSize
  -- Gets the size (width, height) of a rectangle item.
  getSize = getItemSize

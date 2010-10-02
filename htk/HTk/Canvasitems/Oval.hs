-- | HTk\'s /oval/ canvas item.
-- An oval object on a canvas widget.
module HTk.Canvasitems.Oval (

  Oval,
  createOval

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

-- | The @Oval@ datatype.
newtype Oval = Oval GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new oval item.
createOval :: Canvas
   -- ^ the parent canvas.
   -> [Config Oval]
   -- ^ the list of configuration options for this oval item.
   -> IO Oval
   -- ^ An oval item.
createOval cnv cnf = createCanvasItem cnv OVAL Oval cnf [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Oval where
  toGUIObject (Oval w) = w
  cname _ = "Oval"

-- | An oval item can be destroyed.
instance Destroyable Oval where
  -- Destroys an oval item.
  destroy = destroy . toGUIObject

-- | You can synchronize on an oval item.
instance Synchronized Oval where
  -- Synchronizes on an oval item.
  synchronize = synchronize . toGUIObject

-- | An oval item is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem Oval

-- | An oval item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem Oval

-- | An oval item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem Oval

-- | An alternative way to specify an oval\'s coords.
instance HasGeometry Oval where
  -- Sets the oval\'s geometry (width, height, upper left position).
  geometry = itemGeo
  -- Gets the oval\'s geometry (width, height, upper left position).
  getGeometry = getGeo

-- | You can specify the (upper left) position of an oval.
instance HasPosition Oval where
  -- Sets the oval\'s (upper left) position.
  position = itemPosition
  -- Gets the (upper left) position of the oval item.
  getPosition = getItemPosition

-- | You can specify the size of an oval item.
instance HasSize Oval where
  -- Sets the width of an oval item.
  width = itemWidth
  -- Gets the width of an oval item.
  getWidth = getItemWidth
  -- Sets the height of an oval item.
  height = itemHeight
  -- Gets the height of an oval item.
  getHeight = getItemHeight
  -- Sets the size (width, height) of an oval item.
  size = itemSize
  -- Sets the size (width, height) of an oval item.
  getSize = getItemSize

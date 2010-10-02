-- | HTk\'s /arc/ canvas item.
-- An arc object on a canvas widget.
module HTk.Canvasitems.Arc (

  Arc,
  createArc,

  extent,
  getExtent,

  start,
  getStart

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Canvasitems.CanvasItem
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.CanvasItemAux
import Util.Computation
import Events.Synchronized
import Events.Destructible


-- -----------------------------------------------------------------------
-- arc
-- -----------------------------------------------------------------------

-- | The @Arc@ datatype.
data Arc = Arc GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new arc item.
createArc :: Canvas
   -- ^ the parent canvas.
   -> [Config Arc]
   -- ^ the list of configuration options for this arc.
   -> IO Arc
   -- ^ An arc item.
createArc cnv cnf =
  createCanvasItem cnv ARC Arc cnf [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Arc where
  toGUIObject (Arc w) = w
  cname _ = "Arc"

-- | An arc item can be destroyed.
instance Destroyable Arc where
  -- Destroys an arc item.
  destroy = destroy . toGUIObject

-- | You can synchronize on an arc item.
instance Synchronized Arc where
  -- Synchronizes on an arc item.
  synchronize = synchronize . toGUIObject

-- | An arc is a canvas item (any canvas item is an instance of the abstract
-- @class CanvasItem@).
instance CanvasItem Arc

-- | An arc item can have several tags (handlers for a set of canvas items).
instance TaggedCanvasItem Arc

-- | An arc is a filled canvas item (it has filling, outline, outline width,
-- and stipple configurations).
instance FilledCanvasItem Arc

-- | An alternative way to specify arc\'s coords.
instance HasGeometry Arc where
  -- Sets the arc\'s geometry (width, height, upper left position).
  geometry    = itemGeo
  -- Gets the arcs geometry (width, height, upper left position).
  getGeometry = getGeo

-- | You can specify the (upper left) position of an arc.
instance HasPosition Arc where
  -- Sets the arc\'s (upper left) position.
  position    = itemPosition
  -- Gets the (upper left) position of the arc.
  getPosition = getItemPosition

-- | You can specify the size of an arc.
instance HasSize Arc where
  -- Sets the width of an arc.
  width       = itemWidth
  -- Gets the width of an arc.
  getWidth    = getItemWidth
  -- Sets the height of an arc.
  height      = itemHeight
  -- Gets the height of an arc.
  getHeight   = getItemHeight
  -- Sets the size (width, height) of an arc.
  size        = itemSize
  -- Gets the size (width, height) of an arc.
  getSize     = getItemSize


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

type Degree = Double

-- | Sets the length of an arc in counter-clockwise direction.
extent :: Degree -> Config Arc
extent d w = cset w "extent" d

-- | Gets the length of an arc in counter-clockwise direction.
getExtent :: Arc -> IO Degree
getExtent w = cget w "extent"

-- | Sets the starting angle of an arc.
start :: Degree -> Config Arc
start d w = cset w "start" d

-- | Gets the starting angle of an arc.
getStart :: Arc -> IO Degree
getStart w = cget w "start"


-- | HTk\'s /polygon/ canvas item.
-- A polygon object on a canvas widget.
module Polygon (

  module CanvasItem,

  Polygon,
  createPolygon

) where

import Core
import CanvasItem
import CanvasTag
import CanvasItemAux
import Synchronized
import Computation
import Destructible


-- -----------------------------------------------------------------------
-- Polygon
-- -----------------------------------------------------------------------

-- | The @Polygon@ datatype.
newtype Polygon = Polygon GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- Constructor
-- -----------------------------------------------------------------------

-- | Constructs a new polygon item.
createPolygon :: Canvas 
   -- ^ the parent canvas.
   -> [Config Polygon] 
   -- ^ the list of configuration options for this polygon
   -- item.
   -> IO Polygon
   -- ^ A polygon item.
createPolygon cnv cnf =
  createCanvasItem cnv POLYGON Polygon cnf [(-1,-1),(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Polygon where 
  -- | Internal.
  toGUIObject (Polygon w) = w
  -- | Internal.
  cname _ = "Polygon"

-- | An polygon item can be destroyed.
instance Destroyable Polygon where
  -- | Destroys a polygon item.
  destroy = destroy . toGUIObject

-- | You can synchronize on a polygon item.
instance Synchronized Polygon where
  -- | Synchronizes on a polygon item.
  synchronize w = synchronize (toGUIObject w)

-- | A polygon item is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem Polygon

-- | A polygon item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem Polygon

-- | A polygon item is a filled canvas item (it has filling, outline,
-- outline width, and stipple configurations).
instance FilledCanvasItem Polygon

-- | A line is a segmented canvas item. It has a splinesteps and smooth
-- configuration.
instance SegmentedCanvasItem Polygon

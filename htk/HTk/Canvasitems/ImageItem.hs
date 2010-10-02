-- | HTk\'s /image/ canvas item.
-- An image object on a canvas widget.
module HTk.Canvasitems.ImageItem (

  ImageItem,
  createImageItem

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Canvasitems.CanvasItem
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.CanvasItemAux
import HTk.Components.Image
import Util.Computation
import Events.Synchronized
import Events.Destructible


-- -----------------------------------------------------------------------
-- ImageItem
-- -----------------------------------------------------------------------

-- | The @ImageItem@ datatype.
newtype ImageItem = ImageItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new bitmap item.
createImageItem :: Canvas
   -- ^ the parent canvas.
   -> [Config ImageItem]
   -- ^ the list of configuration options for this image
   -- item.
   -> IO ImageItem
   -- ^ An image item.
createImageItem cnv cnf =
  createCanvasItem cnv IMAGEITEM ImageItem cnf [(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject ImageItem where
  toGUIObject (ImageItem w) = w
  cname _ = "ImageItem"

-- | An image item can be destroyed.
instance Destroyable ImageItem where
  -- Destroys an image item.
  destroy = destroy . toGUIObject

-- | An image item is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem ImageItem

-- | A image item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem ImageItem

-- | You can specify the position of an image item.
instance HasPosition ImageItem where
  -- Sets the position of the image item.
  position    = itemPositionD2
  -- Gets the position of the image item.
  getPosition = getItemPositionD2

-- | You can specify the anchor position of an image item.
instance HasCanvAnchor ImageItem where
  -- Sets the anchor position of an image item.
  canvAnchor a w = cset w "anchor" a
  -- Gets the anchor position of an image item.
  getCanvAnchor w = cget w "anchor"

-- | An image item is a container for an image object.
instance HasPhoto ImageItem

-- | You can synchronize on an image item.
instance Synchronized ImageItem where
  -- Synchronizes on an image item.
  synchronize = synchronize . toGUIObject

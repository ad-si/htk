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
-- HTk's <strong>image</strong> canvas item.<br>
-- An image object on a canvas widget.
module ImageItem (
  module CanvasItem,

  ImageItem,
  createImageItem
 
) where

import Core
import Configuration
import CanvasItem
import CanvasTag
import CanvasItemAux
import Image
import Computation
import Synchronized
import Destructible


-- -----------------------------------------------------------------------
-- ImageItem
-- -----------------------------------------------------------------------

---
-- The <code>ImageItem</code> datatype.
newtype ImageItem = ImageItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new bitmap item.
-- @param cnv     - the parent canvas.
-- @param cnf     - the list of configuration options for this image
--                  item.
-- @return result - An image item.
createImageItem :: Canvas -> [Config ImageItem] -> IO ImageItem
createImageItem cnv cnf =
  createCanvasItem cnv IMAGEITEM ImageItem cnf [(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject ImageItem where 
---
-- Internal.
  toGUIObject (ImageItem w) = w
---
-- Internal.
  cname _ = "ImageItem"

---
-- An image item can be destroyed.
instance Destroyable ImageItem where
---
-- Destroys an image item.
  destroy = destroy . toGUIObject

---
-- An image item is a canvas item (any canvas item is an instance of the
-- abstract <code>class CanvasItem</code>).
instance CanvasItem ImageItem

---
-- A image item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem ImageItem

---
-- You can specify the position of an image item.
instance HasPosition ImageItem where
---
-- Sets the position of the image item.
  position    = itemPositionD2
---
-- Gets the position of the image item.
  getPosition = getItemPositionD2

---
-- You can specify the anchor position of an image item.
instance HasCanvAnchor ImageItem where
---
-- Sets the anchor position of an image item.
  canvAnchor a w = cset w "anchor" a
---
-- Gets the anchor position of an image item.
  getCanvAnchor w = cget w "anchor"

---
-- An image item is a container for an image object.
instance HasPhoto ImageItem

---
-- You can synchronize on an image item.
instance Synchronized ImageItem where
---
-- Synchronizes on an image item.
  synchronize = synchronize . toGUIObject

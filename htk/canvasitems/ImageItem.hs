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

newtype ImageItem = ImageItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

createImageItem :: Canvas -> [Config ImageItem] -> IO ImageItem
createImageItem cnv ol =
  createCanvasItem cnv IMAGEITEM ImageItem ol [(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIObject ImageItem where 
  toGUIObject (ImageItem w) = w
  cname _ = "ImageItem"

instance Destroyable ImageItem where
  destroy   = destroy . toGUIObject

instance CanvasItem ImageItem

instance TaggedCanvasItem ImageItem

instance HasPosition ImageItem where
  position    = itemPositionD2
  getPosition = getItemPositionD2

instance HasCanvAnchor ImageItem where
  canvAnchor a w = cset w "anchor" a
  getCanvAnchor w = cget w "anchor"

instance HasPhoto ImageItem

instance Synchronized ImageItem where
  synchronize w = synchronize (toGUIObject w)

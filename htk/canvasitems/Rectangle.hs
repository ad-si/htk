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
-- Rectangle
-- -----------------------------------------------------------------------

data Rectangle = Rectangle GUIOBJECT


-- -----------------------------------------------------------------------
-- Constructor
-- -----------------------------------------------------------------------

createRectangle :: Canvas -> [Config Rectangle] -> IO Rectangle
createRectangle cnv ol =
  createCanvasItem cnv RECTANGLE Rectangle ol [(0,0),(0,0)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

instance Eq Rectangle where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject Rectangle where 
        toGUIObject (Rectangle w) = w
        cname _ = "Rectangle"

instance Destroyable Rectangle where
        destroy   = destroy . toGUIObject

instance Synchronized Rectangle where
        synchronize w = synchronize (toGUIObject w)

instance CanvasItem Rectangle

instance TaggedCanvasItem Rectangle

instance FilledCanvasItem Rectangle

instance HasGeometry Rectangle where
        geometry    = itemGeo
        getGeometry = getGeo

instance HasPosition Rectangle where
        position    = itemPosition
        getPosition = getItemPosition

instance HasSize Rectangle where
        width       = itemWidth
        getWidth    = getItemWidth
        height      = itemHeight
        getHeight   = getItemHeight
        size        = itemSize
        getSize     = getItemSize

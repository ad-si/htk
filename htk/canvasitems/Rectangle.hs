{- #########################################################################

MODULE        : Rectangle
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Rectangle (
        module CanvasItem,

        Rectangle,
        newRectangle

        ) where

import Concurrency
import GUICore

import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)
        

-- --------------------------------------------------------------------------
-- Rectangle
-- --------------------------------------------------------------------------

data Rectangle = Rectangle GUIOBJECT


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newRectangle :: [Config Rectangle] -> IO Rectangle
newRectangle ol = createCanvasItem RECTANGLE Rectangle ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance Eq Rectangle where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject Rectangle where 
        toGUIObject (Rectangle w) = w
        cname _ = "Rectangle"

instance Destructible Rectangle where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Rectangle

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


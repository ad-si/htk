{- #######################################################################

MODULE        : Polygon
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 

   #################################################################### -}


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

newtype Polygon = Polygon GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- Constructor
-- -----------------------------------------------------------------------

createPolygon :: Canvas -> [Config Polygon] -> IO Polygon
createPolygon cnv ol =
  createCanvasItem cnv POLYGON Polygon ol [(0,0),(0,0),(0,0)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

instance GUIObject Polygon where 
        toGUIObject (Polygon w) = w
        cname _ = "Polygon"

instance Destroyable Polygon where
        destroy   = destroy . toGUIObject

instance Synchronized Polygon where
        synchronize w = synchronize (toGUIObject w)

instance CanvasItem Polygon

instance TaggedCanvasItem Polygon

instance FilledCanvasItem Polygon

instance SegmentedCanvasItem Polygon

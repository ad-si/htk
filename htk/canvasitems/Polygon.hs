{- #########################################################################

MODULE        : Polygon
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 

   ######################################################################### -}


module Polygon (
        module CanvasItem,

        Polygon,
        newPolygon
        ) where

import Concurrency
import GUICore

import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)


-- --------------------------------------------------------------------------
-- Polygon
-- --------------------------------------------------------------------------

newtype Polygon = Polygon GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newPolygon :: [Config Polygon] -> IO Polygon
newPolygon ol = createCanvasItem POLYGON Polygon ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject Polygon where 
        toGUIObject (Polygon w) = w
        cname _ = "Polygon"

instance Destructible Polygon where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Polygon

instance Synchronized Polygon where
        synchronize w = synchronize (toGUIObject w)

instance CanvasItem Polygon where 
        defaultCoord w = [(0,0),(0,0),(0,0)]

instance TaggedCanvasItem Polygon

instance FilledCanvasItem Polygon

instance SegmentedCanvasItem Polygon



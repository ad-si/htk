{- #######################################################################

MODULE        : Oval
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Canvas Widget & Canvas Tags & Embedded Windows

   #################################################################### -}


module Oval (

        module CanvasItem,

        Oval,
        createOval

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
-- Oval
-- -----------------------------------------------------------------------

newtype Oval = Oval GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- Constructor
-- -----------------------------------------------------------------------

createOval :: Canvas -> [Config Oval] -> IO Oval
createOval cnv ol = createCanvasItem cnv OVAL Oval ol [(0,0),(0,0)]


-- -----------------------------------------------------------------------
-- Instantiations
-- -----------------------------------------------------------------------

instance GUIObject Oval where 
        toGUIObject (Oval w) = w
        cname _ = "Oval"

instance Destroyable Oval where
        destroy   = destroy . toGUIObject

instance Synchronized Oval where
        synchronize w = synchronize (toGUIObject w)

instance CanvasItem Oval

instance TaggedCanvasItem Oval

instance FilledCanvasItem Oval

instance HasGeometry Oval where
        geometry    = itemGeo
        getGeometry = getGeo

instance HasPosition Oval where
        position    = itemPosition
        getPosition = getItemPosition

instance HasSize Oval where
        width       = itemWidth
        getWidth    = getItemWidth
        height      = itemHeight
        getHeight   = getItemHeight
        size        = itemSize
        getSize     = getItemSize

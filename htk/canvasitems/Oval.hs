{- #########################################################################

MODULE        : Oval
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Canvas Widget & Canvas Tags & Embedded Windows


   ######################################################################### -}


module Oval (
        module CanvasItem,

        Oval,
        newOval

        ) where

import Concurrency
import GUICore

import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)


-- --------------------------------------------------------------------------
-- Oval
-- --------------------------------------------------------------------------

newtype Oval = Oval GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newOval :: [Config Oval] -> IO Oval
newOval ol = createCanvasItem OVAL Oval ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject Oval where 
        toGUIObject (Oval w) = w
        cname _ = "Oval"

instance Destructible Oval where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Oval

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



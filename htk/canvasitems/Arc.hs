{- #########################################################################

MODULE        : Arc
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Arc Item


   ######################################################################### -}


module Arc (
        module CanvasItem,

        Arc,
        newArc,

        extent,
        getExtent,

        start,
        getStart

        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)

        
-- --------------------------------------------------------------------------
-- Arc
-- --------------------------------------------------------------------------

data Arc = Arc GUIOBJECT

        
-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newArc :: [Config Arc] -> IO Arc
newArc ol = createCanvasItem ARC Arc ol

        
-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject Arc where 
        toGUIObject (Arc w) = w
        cname _ = "Arc"

instance Destructible Arc where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Arc

instance Synchronized Arc where
        synchronize w = synchronize (toGUIObject w)

instance CanvasItem Arc

instance TaggedCanvasItem Arc

instance FilledCanvasItem Arc

instance HasGeometry Arc where
        geometry    = itemGeo
        getGeometry = getGeo

instance HasPosition Arc where
        position    = itemPosition
        getPosition = getItemPosition

instance HasSize Arc where
        width       = itemWidth
        getWidth    = getItemWidth
        height      = itemHeight
        getHeight   = getItemHeight
        size        = itemSize
        getSize     = getItemSize

        
-- --------------------------------------------------------------------------
-- Config Options
-- --------------------------------------------------------------------------

type Degree = Double 

extent :: Degree -> Config Arc
extent d w = cset w "extent" d

getExtent :: Arc -> IO Degree
getExtent w = cget w "extent"

start :: Degree -> Config Arc
start d w = cset w "start" d

getStart :: Arc -> IO Degree
getStart w = cget w "start"

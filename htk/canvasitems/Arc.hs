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

module Arc (

  module CanvasItem,

  Arc,
  createArc,

  extent,
  getExtent,

  start,
  getStart

) where

import Core
import Configuration
import CanvasItem
import CanvasTag
import CanvasItemAux
import Computation
import Synchronized
import Destructible


-- -----------------------------------------------------------------------
-- arc
-- -----------------------------------------------------------------------

data Arc = Arc GUIOBJECT


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

createArc :: Canvas -> [Config Arc] -> IO Arc
createArc cnv ol =
  createCanvasItem cnv ARC Arc ol [(-1,-1),(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIObject Arc where 
  toGUIObject (Arc w) = w
  cname _ = "Arc"

instance Destroyable Arc where
  destroy   = destroy . toGUIObject

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


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

type Degree = Double

extent :: Degree -> Config Arc
extent d w = cset w "extent" d

getExtent :: Arc -> IO Degree
getExtent w = cget w "extent"

start :: Degree -> Config Arc
start d w = cset w "start" d

getStart :: Arc -> IO Degree
getStart w = cget w "start"

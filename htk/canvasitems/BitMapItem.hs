{- #######################################################################

MODULE        : BitMapItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : BitMap Item

TO BE DONE    : anchor config option

   #################################################################### -}


module BitMapItem (
  module CanvasItem,

  BitMapItem,
  createBitMapItem

) where

import Core
import Configuration
import Colour(toColour)
import CanvasItem
import CanvasTag
import CanvasItemAux
import BitMap
import Computation
import Synchronized
import Destructible


-- -----------------------------------------------------------------------
-- BitMapItem
-- -----------------------------------------------------------------------

newtype BitMapItem = BitMapItem GUIOBJECT deriving Eq

-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

createBitMapItem :: Canvas -> [Config BitMapItem] -> IO BitMapItem
createBitMapItem cnv ol =
  createCanvasItem cnv BITMAPITEM BitMapItem ol [(0,0)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIObject BitMapItem where 
  toGUIObject (BitMapItem w) = w
  cname _ = "BitMapItem"

instance Destroyable BitMapItem where
  destroy   = destroy . toGUIObject

instance CanvasItem BitMapItem

instance TaggedCanvasItem BitMapItem

instance HasPosition BitMapItem where
  position        = itemPositionD2
  getPosition     = getItemPositionD2

instance HasCanvAnchor BitMapItem where
  canvAnchor a w = cset w "anchor" a
  getCanvAnchor w = cget w "anchor"

instance FilledCanvasItem BitMapItem where
  filling c w       = cset w "foreground" (toColour c)
  getFilling w      = cget w "foreground"
  outline c w       = cset w "background" (toColour c)
  getOutline w      = cget w "background"
  outlinewidth c w  = return w
  getOutlineWidth w = return cdefault
  stipple b w       = setBitMapHandle w "bitmap" b True
  getStipple w      = getBitMapHandle w "bitmap"

instance HasBitMap BitMapItem

instance Synchronized BitMapItem where
  synchronize w = synchronize (toGUIObject w)

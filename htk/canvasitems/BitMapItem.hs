{- #########################################################################

MODULE        : BitMapItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : BitMap Item

TO BE DONE    : anchor config option

   ######################################################################### -}


module BitMapItem (
        module CanvasItem,

        BitMapItem,
        newBitMapItem
        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import BitMap
import Debug(debug)

-- --------------------------------------------------------------------------
-- BitMapItem
-- --------------------------------------------------------------------------

newtype BitMapItem = BitMapItem GUIOBJECT deriving Eq

-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newBitMapItem :: [Config BitMapItem] -> IO BitMapItem
newBitMapItem ol = createCanvasItem BITMAPITEM BitMapItem ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject BitMapItem where 
        toGUIObject (BitMapItem w) = w
        cname _ = "BitMapItem"


instance Destructible BitMapItem where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive BitMapItem

instance CanvasItem BitMapItem where 
        defaultCoord w = [(0,0)]

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




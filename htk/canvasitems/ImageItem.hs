{- #########################################################################

MODULE        : ImageItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 

TO BE dONE    : anchor config option


   ######################################################################### -}


module ImageItem (
        module CanvasItem,

        ImageItem,
        newImageItem
 
        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import Image
import Debug(debug)


-- --------------------------------------------------------------------------
-- ImageItem
-- --------------------------------------------------------------------------

newtype ImageItem = ImageItem GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newImageItem :: [Config ImageItem] -> IO ImageItem
newImageItem ol = createCanvasItem IMAGEITEM ImageItem ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject ImageItem where 
        toGUIObject (ImageItem w) = w
        cname _ = "ImageItem"

instance Destructible ImageItem where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive ImageItem

instance CanvasItem ImageItem where 
        defaultCoord w = [(0,0)]

instance TaggedCanvasItem ImageItem

instance HasPosition ImageItem where
        position    = itemPositionD2
        getPosition = getItemPositionD2

instance HasCanvAnchor ImageItem where
	canvAnchor a w = cset w "anchor" a
	getCanvAnchor w = cget w "anchor"

instance HasPhoto ImageItem

instance Synchronized ImageItem where
        synchronize w = synchronize (toGUIObject w)


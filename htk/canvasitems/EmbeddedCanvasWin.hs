{- #########################################################################

MODULE        : EmbeddedCanvasWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Canvas Embedded Windows


   ######################################################################### -}


module EmbeddedCanvasWin (
        module CanvasItem,

        EmbeddedCanvasWin,
        newEmbeddedCanvasWin 
        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)


-- --------------------------------------------------------------------------
-- Embedded Window
-- --------------------------------------------------------------------------

newtype EmbeddedCanvasWin = EmbeddedCanvasWin GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Construction
-- --------------------------------------------------------------------------

newEmbeddedCanvasWin :: Widget w => w -> [Config EmbeddedCanvasWin] -> 
                        IO EmbeddedCanvasWin 
newEmbeddedCanvasWin w ol = do {
        wid <- createGUIObject 
                        (CANVASITEM EMBEDDEDCANVASWIN []) canvasitemMethods;
        makeChildObject wid (toGUIObject w);
        ewin <- return (EmbeddedCanvasWin wid);
        configure ewin ((coord (defaultCoord ewin)) :ol);
}


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance GUIObject EmbeddedCanvasWin where 
        toGUIObject (EmbeddedCanvasWin w) = w
        cname _         = "EmbeddedCanvasWin"

instance Destructible EmbeddedCanvasWin where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive EmbeddedCanvasWin

instance CanvasItem EmbeddedCanvasWin where 
        defaultCoord w  = [(0,0)]

instance TaggedCanvasItem EmbeddedCanvasWin

instance HasPosition EmbeddedCanvasWin where
        position        = itemPositionD2
        getPosition     = getItemPositionD2

instance HasSize EmbeddedCanvasWin

instance Widget EmbeddedCanvasWin where
        cursor s w      = return w
        getCursor w     = return cdefault
        takeFocus b w   = return w
        getTakeFocus w  = return cdefault
        fill f w        = return w
        expand b w      = return w
        anchor a w      = cset w "anchor" a
        getAnchor w     = cget w "anchor"

instance Synchronized EmbeddedCanvasWin where
        synchronize w = synchronize (toGUIObject w)





{- #########################################################################

MODULE        : TextItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Text items


   ######################################################################### -}


module TextItem (
        module CanvasItem,

        TextItem,
        newTextItem
        ) where

import Concurrency
import GUICore
import CanvasItem
import CanvasTag
import CanvasItemAux
import Debug(debug)


-- --------------------------------------------------------------------------
-- TextItem
-- --------------------------------------------------------------------------

newtype TextItem a = TextItem GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newTextItem :: [Config (TextItem a)] -> IO (TextItem a)
newTextItem ol = createCanvasItem TEXTITEM TextItem ol


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject (TextItem a) where 
        toGUIObject (TextItem w) = w
        cname _ = "TextItem"

instance Destructible (TextItem a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (TextItem a)

instance CanvasItem (TextItem a) where 
        defaultCoord w = [(0,0)]

instance FilledCanvasItem (TextItem a) where
        outline c w  = return w
        getOutline w = return cdefault

instance TaggedCanvasItem (TextItem a)

instance HasPosition (TextItem a) where
        position     = itemPositionD2
        getPosition  = getItemPositionD2

instance HasSize (TextItem a) where
        height _ w  = return w
        getHeight _ = return 1

instance GUIValue a => HasJustify (TextItem a)

instance HasFont (TextItem a)

-- TBD: instance HasAnchor (TextItem a) 

instance Synchronized (TextItem a) where
        synchronize w = synchronize (toGUIObject w)

instance GUIValue a => Variable TextItem a where
        setVar w t = cset w "text" t >> done
        getVar w   = cget w "text"
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })
 






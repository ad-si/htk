{- #########################################################################

MODULE        : Label
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}


module Label (
        Label,

        newLabel

        ) where

import Concurrency
import GUICore
import Packer
import Image
import BitMap
import Debug(debug)

-- --------------------------------------------------------------------------
-- Type Label 
-- --------------------------------------------------------------------------           
newtype Label a = Label GUIOBJECT deriving Eq

-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
newLabel :: [Config (Label a)] -> IO (Label a)
newLabel ol = do 
        w <- createWidget LABEL
        configure (Label w) ol

                                
-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance GUIObject (Label a) where 
        toGUIObject (Label w) = w
        cname _ = "Label"

instance Destructible (Label a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Label a)

instance Widget (Label a)

instance ChildWidget (Label a)

instance Synchronized (Label a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBitMap (Label BitMap)

instance HasBorder (Label a)

instance HasColour (Label a) where 
        legalColourID = hasForeGroundColour

instance HasFont (Label a)

instance HasJustify (Label a)

instance HasPhoto (Label Image)

instance HasSize (Label a)

instance HasUnderline (Label a)

instance GUIValue a => Variable Label a where
        setVar w t = cset w "text" t >> done
        getVar w   = cget w "text"
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

instance GUIValue b => HasText (Label a) b where -- only for the brave
        text t w   = cset w "text" t
        getText w  = cget w "text"


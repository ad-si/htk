{- #########################################################################

MODULE        : Icon
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}

module Icon (
        Window, 

        Icon(..),
        iconMask,
        getIconMask

        ) where

import Concurrency
import GUICore
import BitMap
import Debug(debug)


-- --------------------------------------------------------------------------
-- Type Icon's 
-- --------------------------------------------------------------------------           
newtype Icon = Icon Window deriving (Eq,Ord)

-- --------------------------------------------------------------------------
-- Instantions 
-- --------------------------------------------------------------------------           
instance GUIObject Icon where
        toGUIObject (Icon win) = toGUIObject win
        cname _ = "Icon"
        cset (Icon win) cid val = cset win cid val >> return (Icon win)
        cget (Icon win) cid = cget win cid

instance Destructible Icon where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance HasBitMap Icon where
        bitmap s icon   = setBitMapHandle icon "iconbitmap" (toBitMap s) False
        getBitMap icon  = getBitMapHandle icon "iconbitmap"

instance GUIValue v => HasText Icon v where
        text s icon  = cset icon "iconname" s
        getText icon = cget icon "iconname"

instance HasPosition Icon where
        position p icon = cset icon "iconposition" p
        getPosition icon = tkcget icon "iconposition"

instance Synchronized Icon where
        synchronize w = synchronize (toGUIObject w)
                                        

-- --------------------------------------------------------------------------
-- Config Options 
-- --------------------------------------------------------------------------           
iconMask :: BitMapDesignator h => h -> Config Icon
iconMask s icon =  setBitMapHandle icon "iconmask" (toBitMap s) False

getIconMask :: Icon -> IO BitMapHandle
getIconMask icon = getBitMapHandle icon "iconmask"

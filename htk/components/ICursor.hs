{- #########################################################################

MODULE        : ICursor
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : . 


   ######################################################################### -}


module ICursor (

        module Index,

        ICursor(..),
        HasInsertionCursor(..),
        HasInsertionCursorIndexGet(..),
        HasInsertionCursorIndexSet(..),

        insertOffTime,
        getInsertOffTime,

        insertOnTime,
        getInsertOnTime

) where

import Concurrency
import GUICore
import Index
import Debug(debug)


-- --------------------------------------------------------------------------
-- Classes for  Insertion Cursor 
-- --------------------------------------------------------------------------

class Widget w => HasInsertionCursor w


class HasInsertionCursor w => HasInsertionCursorIndexSet w i where
        insertionCursor    :: i -> Config w


class HasInsertionCursor w => HasInsertionCursorIndexGet w i 
 where
        getInsertionCursor :: w -> IO i


-- --------------------------------------------------------------------------
-- Handle 
-- --------------------------------------------------------------------------

newtype HasInsertionCursor w => ICursor w = ICursor w


-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance GUIObject w => GUIObject (ICursor w) where
        toGUIObject (ICursor w) = toGUIObject w
        cname (ICursor w) = cname w

instance (HasInsertionCursor w,Widget w) => HasColour (ICursor w) where
        legalColourID = hasBackGroundColour
        setColour w "bg" c = cset w "insertbackground" (toColour c)
        setColour w _ _ = return w
        getColour w "bg" = cget w "insertbackground"
        getColour _ _ = return cdefault


instance (HasInsertionCursor w,Widget w) => HasBorder (ICursor w) where
        borderwidth s w = cset w "insertborderwidth" s
        getBorderwidth w = cget w "insertborderwidth"
        relief _ w = return w
        getRelief _ = return Raised 

instance (HasInsertionCursor w,Widget w) => HasSize (ICursor w) where
        width s w   = cset w "insertwidth" s
        getWidth w  = cget w "insertwidth"
        height h w  = return w
        getHeight w = return cdefault


-- --------------------------------------------------------------------------
-- Config Options
-- --------------------------------------------------------------------------

insertOffTime :: HasInsertionCursor w => Int -> Config (ICursor w)
insertOffTime i w = cset w  "insertofftime" i

getInsertOffTime :: HasInsertionCursor w => ICursor w -> IO Int
getInsertOffTime w = cget w "insertofftime"

insertOnTime :: HasInsertionCursor w => Int -> Config (ICursor w)
insertOnTime i w = cset w "insertontime" i

getInsertOnTime :: HasInsertionCursor w => (ICursor w) -> IO Int
getInsertOnTime w = cget w "insertontime"

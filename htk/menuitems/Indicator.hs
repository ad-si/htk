{- #########################################################################

MODULE        : Indicator
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 

   ######################################################################### -}


module Indicator (

        Indicator(..),

        HasColour(..),
        HasPhoto(..),
        SelectButton(..),
        HasIndicator(..),
        ) where


import Concurrency
import GUICore

import EventLoop
import Image
import BitMap
import Packer
import MenuItem
import Debug(debug)


-- --------------------------------------------------------------------------
-- Handle
-- --------------------------------------------------------------------------   

data Indicator a = Indicator a


-- --------------------------------------------------------------------------
-- Class HasIndicator
-- --------------------------------------------------------------------------   

class Widget w => HasIndicator w where
        indicator       :: Toggle -> Config w
        getIndicator    :: w -> IO Toggle
        indicator i w    = cset w "indicatoron" i
        getIndicator w   = cget w "indicatoron"

-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------   

instance HasIndicator w => GUIObject (Indicator w) where
        toGUIObject (Indicator w) = toGUIObject w

instance (HasIndicator w, SelectButton w) => HasColour (Indicator w) where
        setColour w _ c = cset w "selectcolor" (toColour c)
        getColour w _   = cget w "selectcolor"

instance (HasIndicator w, SelectButton w) => HasPhoto (Indicator w) where
        photo i w   = imageToInt i >>= cset w "selectimage"
        getPhoto w  = cget w "selectimage" >>= intToImage

-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance HasIndicator GUIOBJECT


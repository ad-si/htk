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

module Slider (

  Slider(..),
  HasSlider(..)

) where

import Core
import Configuration
import Computation
import BaseClasses(Widget)
import Colour(toColour)


-- -----------------------------------------------------------------------
--  Classes
-- -----------------------------------------------------------------------

class Widget w => HasSlider w where             -- for sliders
        repeatInterval     :: Int -> Config (Slider w)
        getRepeatInterval  :: (Slider w) -> IO Int
        repeatDelay        :: Int -> Config (Slider w)
        getRepeatDelay     :: (Slider w) -> IO Int    
        repeatInterval c w  = cset w "repeatinterval" c
        getRepeatInterval w = cget w "repeatinterval" 
        repeatDelay c w     = cset w "repeatdelay" c
        getRepeatDelay w    = cget w "repeatdelay"    
-- TD:
-- this is from Einar's implementation
-- I dont't think this makes sense for all widgets


-- -----------------------------------------------------------------------
--  Handle
-- -----------------------------------------------------------------------

newtype HasSlider w => Slider w = Slider w


-- -----------------------------------------------------------------------
--  Instantiations
-- -----------------------------------------------------------------------

instance GUIObject w => GUIObject (Slider w) where
        toGUIObject (Slider w)  = toGUIObject w
        cname (Slider w)        = cname w

instance (HasSlider w,GUIObject w) => HasColour (Slider w) where
        legalColourID             = hasForeGroundColour
        setColour w "foreground" c = cset w "troughcolor" (toColour c)
        setColour w "background" c =
          cset w "activebackground" (toColour c)
        setColour w _ _            = return w
        getColour w "background"   = cget w "troughcolor"
        getColour w "foreground"   = cget w "activebackground"
        getColour _ _             = return cdefault

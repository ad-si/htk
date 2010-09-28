-- | The @module Slider@ implements configuration options for
-- widgets with sliders (scale widgets and scrollbars).
module HTk.Components.Slider (

  Slider(..),
  HasSlider(..)

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import Util.Computation
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Colour(toColour)


-- -----------------------------------------------------------------------
-- class HasSlider
-- -----------------------------------------------------------------------

-- | Widgets with sliders (scale widget, scrollbar) instantiate the
-- @class HasSlider@.
class Widget w => HasSlider w where
  -- Sets the time period between auto-repeat events.
  repeatInterval     :: Int -> Config (Slider w)
  -- Gets the time period between auto-repeat events.
  getRepeatInterval  :: (Slider w) -> IO Int
  -- Sets the delay before auto-repeat starts (e.g. when mouse button is
  -- pressed).
  repeatDelay        :: Int -> Config (Slider w)
  -- Gets the delay before auto-repeat starts.
  getRepeatDelay     :: (Slider w) -> IO Int

  repeatInterval c w  = cset w "repeatinterval" c
  getRepeatInterval w = cget w "repeatinterval"
  repeatDelay c w     = cset w "repeatdelay" c
  getRepeatDelay w    = cget w "repeatdelay"


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Slider@ datatype.
newtype Slider w = Slider w


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject w => GUIObject (Slider w) where
  toGUIObject (Slider w)  = toGUIObject w
  cname (Slider w)        = cname w

-- | The slider component has a configureable foreground and background
-- colour.
instance (HasSlider w,GUIObject w) => HasColour (Slider w) where
  legalColourID              = hasForeGroundColour
  setColour w "foreground" c = cset w "troughcolor" (toColour c)
  setColour w "background" c = cset w "activebackground" (toColour c)
  setColour w _ _            = return w
  getColour w "background"   = cget w "troughcolor"
  getColour w "foreground"   = cget w "activebackground"
  getColour _ _              = return cdefault

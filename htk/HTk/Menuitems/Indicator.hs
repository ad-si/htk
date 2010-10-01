-- | HTk\'s menuitem /indicators/.
-- Indicators are displayed with menu checkbuttons, menu buttons and
-- menu radiobuttons.
module HTk.Menuitems.Indicator (
  Indicator(..),

  HasColour(..),
  HasPhoto(..),
  SelectButton(..),
  HasIndicator(..)

) where

import HTk.Kernel.Core
import HTk.Components.Image
import HTk.Menuitems.MenuItem
import Util.Computation
import HTk.Kernel.Resources
import HTk.Kernel.Colour


-- -----------------------------------------------------------------------
-- handle
-- -----------------------------------------------------------------------

-- | The @Indicator@ datatype.
data Indicator a = Indicator a


-- -----------------------------------------------------------------------
-- class HasIndicator
-- -----------------------------------------------------------------------

-- | Menu items that can have an indicator instantiate the
-- @class HasIndicator@.
class GUIObject w => HasIndicator w where
  -- Displays\/unmaps the items indicator.
  indicator       :: Toggle -> Config w
  -- @On@ if an indicator is displayed with the item, otherwise
  -- @Off@.
  getIndicator    :: w -> IO Toggle
  indicator i w    = cset w "indicatoron" i
  getIndicator w   = cget w "indicatoron"


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance HasIndicator w => GUIObject (Indicator w) where
  toGUIObject (Indicator w) = toGUIObject w
  cname _ = "Indicator"

-- | You can specify the colour for the selector of menu checkbuttons and
-- menu radiobuttons.
instance (HasIndicator w, SelectButton w) => HasColour (Indicator w) where
  -- Sets the colour for the selector.
  setColour w _ c = cset w "selectcolor" (toColour c)
  -- Gets the colour for the selector.
  getColour w _   = cget w "selectcolor"

-- | You can specify specify an alternate image for the selector of menu
-- checkbuttons and menu radiobuttons.
instance (HasIndicator w, SelectButton w) => HasPhoto (Indicator w) where
  -- Sets the alternate image for the selector.
  photo i w   = imageToInt i >>= cset w "selectimage"
  -- Gets the alternate image for the selector.
  getPhoto w  = cget w "selectimage" >>= intToImage


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance HasIndicator GUIOBJECT

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

---
-- HTk's menuitem <strong>indicators</strong>.<br>
-- Indicators are displayed with menu checkbuttons, menu buttons and
-- menu radiobuttons.
module Indicator (

  Indicator(..),

  HasColour(..),
  HasPhoto(..),
  SelectButton(..),
  HasIndicator(..)

) where

import Core
import Configuration
import Image
import BitMap
import Menu
import MenuItem
import Computation
import BaseClasses(Widget)
import Resources
import Colour


-- -----------------------------------------------------------------------
-- handle
-- -----------------------------------------------------------------------

---
-- The <code>Indicator</code> datatype.
data Indicator a = Indicator a


-- -----------------------------------------------------------------------
-- class HasIndicator
-- -----------------------------------------------------------------------

---
-- Menu items that can have an indicator instantiate the
-- <code>class HasIndicator</code>.
class GUIObject w => HasIndicator w where
---
-- Displays/unmaps the items indicator.
  indicator       :: Toggle -> Config w
---
-- <code>On</code> if an indicator is displayed with the item, otherwise
-- <code>Off</code>.
  getIndicator    :: w -> IO Toggle
  indicator i w    = cset w "indicatoron" i
  getIndicator w   = cget w "indicatoron"


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance HasIndicator w => GUIObject (Indicator w) where
---
-- Internal.
  toGUIObject (Indicator w) = toGUIObject w

---
-- You can specify the colour for the selector of menu checkbuttons and
-- menu radiobuttons.
instance (HasIndicator w, SelectButton w) => HasColour (Indicator w) where
---
-- Sets the colour for the selector.
  setColour w _ c = cset w "selectcolor" (toColour c)
---
-- Gets the colour for the selector.
  getColour w _   = cget w "selectcolor"

---
-- You can specify specify an alternate image for the selector of menu
-- checkbuttons and menu radiobuttons.
instance (HasIndicator w, SelectButton w) => HasPhoto (Indicator w) where
---
-- Sets the alternate image for the selector.
  photo i w   = imageToInt i >>= cset w "selectimage"
---
-- Gets the alternate image for the selector.
  getPhoto w  = cget w "selectimage" >>= intToImage


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance HasIndicator GUIOBJECT

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

module RadioButton (

  ButtonWidget(..),

  RadioButton,
  newRadioButton

) where

import Core
import ButtonWidget
import BaseClasses(Widget)
import Configuration
import Image
import BitMap
import Concurrent
import Computation
import Destructible
import Synchronized
import TkVariables
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

data RadioButton a = RadioButton GUIOBJECT


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

newRadioButton :: Container par => par -> [Config (RadioButton a)] -> IO (RadioButton a)
newRadioButton par ol =
  do
    b <- createGUIObject (toGUIObject par) RADIOBUTTON defMethods
    configure (RadioButton b) ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq (RadioButton a) where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (RadioButton a) where 
  toGUIObject (RadioButton w) = w
  cname _ = "RadioButton"

instance Destroyable (RadioButton a) where
  destroy   = destroy . toGUIObject

instance Widget (RadioButton a)
 
instance ButtonWidget (RadioButton a)

instance HasBitMap (RadioButton a)

instance HasBorder (RadioButton a)

instance HasColour (RadioButton a) where 
  legalColourID = buttonColours

instance HasEnable (RadioButton a)

instance HasFont (RadioButton a)

instance HasJustify (RadioButton a)

instance HasPhoto (RadioButton a)

instance HasSize (RadioButton a)

instance GUIValue v => HasText (RadioButton a) v

instance HasUnderline (RadioButton a)

instance Synchronized (RadioButton a) where
  synchronize = synchronize . toGUIObject

instance HasCommand (RadioButton a)

instance GUIValue b => HasValue (RadioButton a) b

instance HasVariable (RadioButton a)

---
-- A radio button can have a tooltip.
instance HasTooltip (RadioButton a)

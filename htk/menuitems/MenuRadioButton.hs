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

module MenuRadioButton (

--  HasIndicator(..),
  HasAccelerator(..),

  MenuRadioButton,
  createMenuRadioButton

) where

import Core
import Configuration
import Image
import BitMap
import Menu
import MenuItem
--import Indicator       -- TD ??
import Synchronized
import Computation
import TkVariables


-- -----------------------------------------------------------------------
-- MenuCascade type
-- -----------------------------------------------------------------------

newtype MenuRadioButton = MenuRadioButton GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

createMenuRadioButton :: Menu -> [Config MenuRadioButton] ->
                         IO MenuRadioButton
createMenuRadioButton m ol =
  createMenuItem m MENURADIOBUTTON MenuRadioButton ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance MenuItem MenuRadioButton

instance Eq MenuRadioButton where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MenuRadioButton where 
  toGUIObject (MenuRadioButton w) = w
  cname _ = "MenuRadioButton"

instance Synchronized MenuRadioButton where
  synchronize = synchronize . toGUIObject

instance HasAccelerator MenuRadioButton

instance HasBitMap MenuRadioButton

instance HasBorder MenuRadioButton

instance HasColour MenuRadioButton where 
  legalColourID = buttonColours

instance HasEnable MenuRadioButton

instance HasFont MenuRadioButton

instance HasJustify MenuRadioButton

--instance HasIndicator MenuRadioButton

instance HasPhoto MenuRadioButton

instance HasSize MenuRadioButton

instance GUIValue v => HasText MenuRadioButton v where
  text str w = cset w "label" str >> return w
  getText w = cget w "label"

instance HasUnderline MenuRadioButton

instance HasVariable MenuRadioButton

instance GUIValue v => HasValue MenuRadioButton v

instance HasCommand MenuRadioButton
{- #######################################################################

MODULE        : MenuButton
AUTHOR        : Einar Karlsen,
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 2001
DESCRIPTION   : menu checkbutton

   #################################################################### -}


module MenuCheckButton (

--  HasIndicator(..),
  HasAccelerator(..),

  MenuCheckButton,
  createMenuCheckButton

) where

import Core
import Configuration
import Image
import BitMap
import Menu
import MenuItem
--import Indicator
import Synchronized
import Computation
import TkVariables


-- -----------------------------------------------------------------------
-- MenuCascade type
-- -----------------------------------------------------------------------

newtype MenuCheckButton = MenuCheckButton GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

createMenuCheckButton :: Menu -> [Config MenuCheckButton] ->
                         IO MenuCheckButton
createMenuCheckButton m ol =
  createMenuItem m MENUCHECKBUTTON MenuCheckButton ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance MenuItem MenuCheckButton

instance Eq MenuCheckButton where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MenuCheckButton where 
  toGUIObject (MenuCheckButton w) = w
  cname _ = "MenuCheckButton"

instance Synchronized MenuCheckButton where
  synchronize = synchronize . toGUIObject

instance HasAccelerator MenuCheckButton

instance HasBitMap MenuCheckButton

instance HasBorder MenuCheckButton

instance HasColour MenuCheckButton where 
  legalColourID = buttonColours

instance HasEnable MenuCheckButton

instance HasFont MenuCheckButton

instance HasJustify MenuCheckButton

--instance HasIndicator MenuCheckButton

instance HasPhoto MenuCheckButton

instance HasSize MenuCheckButton

instance GUIValue v => HasText MenuCheckButton v where
  text str w = cset w "label" str >> return w
  getText w = cget w "label"

instance HasUnderline MenuCheckButton

instance HasVariable MenuCheckButton

instance GUIValue v => HasValue MenuCheckButton v

instance HasCommand MenuCheckButton
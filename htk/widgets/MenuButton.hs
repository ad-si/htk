{- #######################################################################

MODULE        : Button
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : button associated with a pulldown menu

   #################################################################### -}


module MenuButton (

  ButtonWidget(..),

  MenuButton,
  newMenuButton

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
import Menu
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

data MenuButton = MenuButton GUIOBJECT


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

newMenuButton :: Container par => par -> [Config MenuButton] -> IO MenuButton
newMenuButton par ol =
  do
    b <- createGUIObject (toGUIObject par) MENUBUTTON defMethods
    configure (MenuButton b) ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq MenuButton where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MenuButton where 
  toGUIObject (MenuButton w) = w
  cname _ = "MenuButton"

instance Destroyable MenuButton where
  destroy   = destroy . toGUIObject

instance Widget MenuButton
 
instance ButtonWidget MenuButton

instance HasBitMap MenuButton

instance HasBorder MenuButton

instance HasColour MenuButton where 
  legalColourID = buttonColours

instance HasEnable MenuButton

instance HasFont MenuButton

instance HasJustify MenuButton

instance HasPhoto MenuButton

instance HasSize MenuButton

instance GUIValue v => HasText MenuButton v

instance HasUnderline MenuButton

instance Synchronized MenuButton where
  synchronize = synchronize . toGUIObject

instance HasCommand MenuButton

instance HasMenu MenuButton

---
-- A menubutton can have a tooltip.
instance HasTooltip MenuButton

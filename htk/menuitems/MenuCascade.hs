{- #######################################################################

MODULE        : MenuButton
AUTHOR        : Einar Karlsen,
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 2001
DESCRIPTION   : cascade

   #################################################################### -}


module MenuCascade (

--  HasIndicator(..),
  HasAccelerator(..),

  MenuCascade,
  createMenuCascade

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


-- -----------------------------------------------------------------------
-- MenuCascade type
-- -----------------------------------------------------------------------

newtype MenuCascade = MenuCascade GUIOBJECT


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance HasMenu MenuCascade


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

createMenuCascade :: Menu -> [Config MenuCascade] -> IO MenuCascade
createMenuCascade m ol = createMenuItem m MENUCASCADE MenuCascade ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance MenuItem MenuCascade

instance Eq MenuCascade where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MenuCascade where 
  toGUIObject (MenuCascade w) = w
  cname _ = "MenuCascade"
                        
instance Synchronized MenuCascade where
  synchronize = synchronize . toGUIObject

instance HasAccelerator MenuCascade

instance HasBitMap MenuCascade

instance HasBorder MenuCascade

instance HasColour MenuCascade where 
  legalColourID = buttonColours

instance HasEnable MenuCascade

instance HasFont MenuCascade

instance HasJustify MenuCascade

--instance HasIndicator MenuCascade

instance HasPhoto MenuCascade

instance HasSize MenuCascade

instance GUIValue v => HasText MenuCascade v where
  text str w = cset w "label" str >> return w
  getText w = cget w "label"

instance HasUnderline MenuCascade
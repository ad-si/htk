{- #######################################################################

MODULE        : MenuButton
AUTHOR        : Einar Karlsen,
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 2001
DESCRIPTION   : menu command

   #################################################################### -}


module MenuCommand (

--  HasIndicator(..),
  HasAccelerator(..),

  MenuCommand,
  createMenuCommand

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
-- MenuCommand type
-- -----------------------------------------------------------------------

newtype MenuCommand = MenuCommand GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

createMenuCommand :: Menu -> [Config MenuCommand] -> IO MenuCommand
createMenuCommand m ol = createMenuItem m MENUCOMMAND MenuCommand ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance MenuItem MenuCommand

instance Eq MenuCommand where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject MenuCommand where 
  toGUIObject (MenuCommand w) = w
  cname _ = "MenuCommand"
                        
instance Synchronized MenuCommand where
  synchronize = synchronize . toGUIObject

instance HasAccelerator MenuCommand

instance HasBitMap MenuCommand

instance HasBorder MenuCommand

instance HasColour MenuCommand where 
  legalColourID = buttonColours

instance HasEnable MenuCommand

instance HasFont MenuCommand

instance HasJustify MenuCommand

--instance HasIndicator MenuCommand

instance HasPhoto MenuCommand

instance HasSize MenuCommand

instance GUIValue v => HasText MenuCommand v where
  text str w = cset w "label" str >> return w
  getText w = cget w "label"

instance HasUnderline MenuCommand

instance HasCommand MenuCommand
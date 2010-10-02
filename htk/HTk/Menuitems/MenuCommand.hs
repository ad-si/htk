{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /menu command/.
-- A simple command inside a menu.
module HTk.Menuitems.MenuCommand (

  MenuCommand,
  createMenuCommand

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Components.BitMap
import HTk.Menuitems.Menu
import HTk.Menuitems.MenuItem
import HTk.Menuitems.Indicator
import Events.Synchronized
import Util.Computation


-- -----------------------------------------------------------------------
-- MenuCommand type
-- -----------------------------------------------------------------------

-- | The @MenuCommand@ datatype.
newtype MenuCommand = MenuCommand GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new menu command and returns a handler.
createMenuCommand :: Menu
   -- ^ the parent menu.
   -> [Config MenuCommand]
   -- ^ the list of configuration options for this menu
   -- command.
   -> IO MenuCommand
   -- ^ A menu command.
createMenuCommand m cnf = createMenuItem m MENUCOMMAND MenuCommand cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A menu command is a menu item (any menu item instantiates the
-- abstract @class MenuItem@).
instance MenuItem MenuCommand

-- | Internal.
instance Eq MenuCommand where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject MenuCommand where
  toGUIObject (MenuCommand w) = w
  cname _ = "MenuCommand"

-- | You can synchronize on a menu command.
instance Synchronized MenuCommand where
  -- Synchronizes on a menu command.
  synchronize = synchronize . toGUIObject

-- | A menu command has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuCommand

-- | A menu command can contain a bitmap (instead of text or an image).
instance HasBitMap MenuCommand

-- | A menu command has a configureable border.
instance HasBorder MenuCommand

-- | A menu command has a normal foreground and background colour and
-- an active\/disabled foreground and background colour.
instance HasColour MenuCommand where
  legalColourID = buttonColours

-- | A menu command is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuCommand

-- | You can specify the font of a menu command.
instance HasFont MenuCommand

-- | A menu command has a configureable text justification.
instance HasJustify MenuCommand

-- | You can display an indicator with a menu command.
instance HasIndicator MenuCommand

-- | A menu command can contain an image (instead of text or a bitmap).
instance HasPhoto MenuCommand

-- | You can specify the size of a menu command.
instance HasSize MenuCommand

-- | A menu command can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuCommand v where
  -- Sets the text to display.
  text str w = cset w "label" str >> return w
  -- Gets the displayed text.
  getText w = cget w "label"

-- | You can set the index of a text character to underline.
instance HasUnderline MenuCommand

-- | When a menu command is clicked, a corresponding event is invoked.
instance HasCommand MenuCommand

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
-- HTk's <strong>menu command</strong>.<br>
-- A simple command inside a menu.
module MenuCommand (

  HasIndicator(..),
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
import Indicator
import Synchronized
import Computation


-- -----------------------------------------------------------------------
-- MenuCommand type
-- -----------------------------------------------------------------------

---
-- The <code>MenuCommand</code> datatype.
newtype MenuCommand = MenuCommand GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

---
-- Constructs a new menu command and returns a handler.
-- @param m       - the parent menu.
-- @param cnf     - the list of configuration options for this menu
--                  command.
-- @return result - A menu command.
createMenuCommand :: Menu -> [Config MenuCommand] -> IO MenuCommand
createMenuCommand m cnf = createMenuItem m MENUCOMMAND MenuCommand cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- A menu command is a menu item (any menu item instantiates the
-- abstract <code>class MenuItem</code>).
instance MenuItem MenuCommand

---
-- Internal.
instance Eq MenuCommand where
---
-- Internal.
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

---
-- Internal.
instance GUIObject MenuCommand where 
---
-- Internal.
  toGUIObject (MenuCommand w) = w
---
-- Internal.
  cname _ = "MenuCommand"
                        
---
-- You can synchronize on a menu command.
instance Synchronized MenuCommand where
---
-- Synchronizes on a menu command.
  synchronize = synchronize . toGUIObject

---
-- A menu command has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuCommand

---
-- A menu command can contain a bitmap (instead of text or an image).
instance HasBitMap MenuCommand

---
-- A menu command has a configureable border.
instance HasBorder MenuCommand

---
-- A menu command has a normal foreground and background colour and
-- an active/disabled foreground and background colour.
instance HasColour MenuCommand where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A menu command is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuCommand

---
-- You can specify the font of a menu command.
instance HasFont MenuCommand

---
-- A menu command has a configureable text justification.
instance HasJustify MenuCommand

---
-- You can display an indicator with a menu command.
instance HasIndicator MenuCommand

---
-- A menu command can contain an image (instead of text or a bitmap).
instance HasPhoto MenuCommand

---
-- You can specify the size of a menu command.
instance HasSize MenuCommand

---
-- A menu command can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuCommand v where
---
-- Sets the text to display.
  text str w = cset w "label" str >> return w
---
-- Gets the displayed text.
  getText w = cget w "label"

---
-- You can set the index of a text character to underline.
instance HasUnderline MenuCommand

---
-- When a menu command is clicked, a corresponding event is invoked.
instance HasCommand MenuCommand

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
-- HTk's <strong>menu cascade item</strong>.<br>
-- A containers for cascaded menus.
module MenuCascade (

  HasIndicator(..),
  HasAccelerator(..),

  MenuCascade,
  createMenuCascade,
  createPulldownMenu

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
-- MenuCascade type
-- -----------------------------------------------------------------------

---
-- The <code>MenuCascade</code> datatype.
newtype MenuCascade = MenuCascade GUIOBJECT


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- A <code>MenuCascade</code> item is a container for a sub-menu.
instance HasMenu MenuCascade


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

---
-- Constructs a new menu cascasde item and returns a handler.
-- @param m       - the parent menu.
-- @param cnf     - the list of configuration options for this menu
--                  cascade item.
-- @return result - A menu cascade item.
createMenuCascade :: Menu -> [Config MenuCascade] -> IO MenuCascade
createMenuCascade m cnf = createMenuItem m MENUCASCADE MenuCascade cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- A menu cascade item is a menu item (any menu item instantiates the
-- abstract <code>class MenuItem</code>).
instance MenuItem MenuCascade

---
-- Internal.
instance Eq MenuCascade where
---
-- Internal.
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

---
-- Internal.
instance GUIObject MenuCascade where 
---
-- Internal.
  toGUIObject (MenuCascade w) = w
---
-- Internal.
  cname _ = "MenuCascade"

---
-- You can synchronize on a menu cascade item.
instance Synchronized MenuCascade where
---
-- Synchronizes on a menu cascade item.
  synchronize = synchronize . toGUIObject

---
-- A menu cascade item has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuCascade

---
-- A menu cascade item can contain a bitmap (instead of text or an image).
instance HasBitMap MenuCascade

---
-- A menu cascade item has a configureable border.
instance HasBorder MenuCascade

---
-- A menu cascade item has a normal foreground and background colour and
-- an active/disabled foreground and background colour.
instance HasColour MenuCascade where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A menu cascade item is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuCascade

---
-- You can specify the font of a menu cascade item.
instance HasFont MenuCascade

---
-- A menu cascade item has a configureable text justification.
instance HasJustify MenuCascade

---
-- You can display an indicator with a menu cascade item.
instance HasIndicator MenuCascade

---
-- A menu cascade item can contain an image (instead of text or a bitmap).
instance HasPhoto MenuCascade

---
-- You can specify the size of a menu cascade item.
instance HasSize MenuCascade

---
-- A menu cascade item can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuCascade v where
---
-- Sets the text to display.
  text str w = cset w "label" str >> return w
---
-- Gets the displayed text.
  getText w = cget w "label"

---
-- You can set the index of a text character to underline.
instance HasUnderline MenuCascade


---
-- Utility function: create a pulldown menu 
--
-- @param mpar    - the parent menu.
-- @param conf    - the list of configuration options for this pulldown menu
-- @return result - A menu cascade item.

createPulldownMenu :: Menu-> [Config MenuCascade]-> IO Menu
createPulldownMenu mpar conf =
  do pd <- createMenuCascade mpar conf
     m  <- createMenu mpar False []
     pd # menu m
     return m


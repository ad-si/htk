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
-- HTk's <strong>menu separator</strong>.<br>
-- A simple separator to group menu entries.
module MenuSeparator (

  MenuSeparator,
  createMenuSeparator

) where

import Core
import Configuration
import Resources
import MenuItem
import Menu
import Synchronized
import Computation


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>MenuSeparator</code> datatype.
data MenuSeparator = MenuSeparator GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new menu separator and returns a handler.
-- @param m       - the parent menu.
-- @param cnf     - the list of configuration options for this menu
--                  separator.
-- @return result - A menu separator.
createMenuSeparator :: Menu -> [Config MenuSeparator] -> IO MenuSeparator
createMenuSeparator m cnf =
  createMenuItem m MENUSEPARATOR MenuSeparator cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- A menu separator is a menu item (any menu item instantiates the
-- abstract <code>class MenuItem</code>).
instance MenuItem MenuSeparator

---
-- Internal.
instance GUIObject MenuSeparator where 
---
-- Internal.
  toGUIObject (MenuSeparator w) = w
---
-- Internal.
  cname w = "MenuSeparator"

---
-- You can synchronize on a menu separator.
instance Synchronized MenuSeparator where
---
-- Synchronizes on a menu separator.
  synchronize = synchronize . toGUIObject

---
-- A menu separator has a configureable border.
instance HasBorder MenuSeparator

---
-- A menu separator has either a vertival or a horizontal orientation.
instance HasOrientation MenuSeparator where
---
-- Sets the menu separators orientation.
  orient Horizontal s = configure s [height 2] >> return s
  orient Vertical s = configure s [width 2] >> return s

---
-- You can specify the size of a menu separator.
instance HasSize MenuSeparator

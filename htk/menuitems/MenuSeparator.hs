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
-- MenuSeparator
-- -----------------------------------------------------------------------

data MenuSeparator = MenuSeparator GUIOBJECT deriving Eq

createMenuSeparator :: Menu -> [Config MenuSeparator] -> IO MenuSeparator
createMenuSeparator m ol =
  createMenuItem m MENUSEPARATOR MenuSeparator ol


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance MenuItem MenuSeparator

instance GUIObject MenuSeparator where 
  toGUIObject (MenuSeparator w) = w
  cname w = "MenuSeparator"

instance Synchronized MenuSeparator where
  synchronize = synchronize . toGUIObject

instance HasBorder MenuSeparator

instance HasOrientation MenuSeparator where
  orient Horizontal s = configure s [height 2] >> return s
  orient Vertical s = configure s [width 2] >> return s

instance HasSize MenuSeparator

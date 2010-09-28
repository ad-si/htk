-- | HTk\'s /menu separator/.
-- A simple separator to group menu entries.
module HTk.Menuitems.MenuSeparator (

  MenuSeparator,
  createMenuSeparator

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Menuitems.MenuItem
import HTk.Menuitems.Menu
import Events.Synchronized
import Util.Computation


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @MenuSeparator@ datatype.
data MenuSeparator = MenuSeparator GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new menu separator and returns a handler.
createMenuSeparator :: Menu
   -- ^ the parent menu.
   -> [Config MenuSeparator]
   -- ^ the list of configuration options for this menu
   -- separator.
   -> IO MenuSeparator
   -- ^ A menu separator.
createMenuSeparator m cnf =
  createMenuItem m MENUSEPARATOR MenuSeparator cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A menu separator is a menu item (any menu item instantiates the
-- abstract @class MenuItem@).
instance MenuItem MenuSeparator

-- | Internal.
instance GUIObject MenuSeparator where
  toGUIObject (MenuSeparator w) = w
  cname w = "MenuSeparator"

-- | You can synchronize on a menu separator.
instance Synchronized MenuSeparator where
  -- Synchronizes on a menu separator.
  synchronize = synchronize . toGUIObject

-- | A menu separator has a configureable border.
instance HasBorder MenuSeparator

-- | A menu separator has either a vertival or a horizontal orientation.
instance HasOrientation MenuSeparator where
  -- Sets the menu separators orientation.
  orient Horizontal s = configure s [height 2] >> return s
  orient Vertical s = configure s [width 2] >> return s

-- | You can specify the size of a menu separator.
instance HasSize MenuSeparator

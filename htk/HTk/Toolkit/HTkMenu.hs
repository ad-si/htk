-- | HTkMenu is a user-friendly interface to HTk's menu operations, which
-- compiles a version of MenuType.MenuPrim to an HTk menu.
module HTk.Toolkit.HTkMenu(
   HTkMenu(..),
   compileHTkMenu,
   ) where

import Util.Computation

import Events.Events

import HTk.Kernel.Core(HasCommand(..))
import HTk.Kernel.Packer(Container)
import HTk.Kernel.Configuration(HasText(..))

import HTk.Menuitems.Menu
import HTk.Widgets.MenuButton
import HTk.Menuitems.MenuCascade
import HTk.Menuitems.MenuSeparator
import HTk.Menuitems.MenuCommand

import HTk.Toolkit.MenuType hiding (MenuPrim(Menu))
import qualified HTk.Toolkit.MenuType as MenuType (MenuPrim(Menu))


-- ----------------------------------------------------------------------
-- The HTkMenu type
-- ----------------------------------------------------------------------

-- | Describes a menu to be compiled.
-- The value identifies the buttons in the menu so the client
-- can tell which was clicked.
-- The String is a title which is given to menu cascades.
newtype HTkMenu value = HTkMenu (MenuType.MenuPrim String value)

-- ----------------------------------------------------------------------
-- compileHTkMenu
-- ----------------------------------------------------------------------

-- | compileHTkMenu compiles a menu to a MenuButton.  It does not display it;
-- the caller should pack the MenuButton in the parent with whatever options
-- are desired.
compileHTkMenu :: Container parent => parent -> HTkMenu value
   -> IO (MenuButton,Event value)
compileHTkMenu parent htkMenu =
   do
      let (title,subMenus) = normalise htkMenu
      menuButton <- newMenuButton parent [text title]
      topMenu <- createMenu menuButton tearoff []
      menuButton # menu topMenu
      clickEvents <- mapM (compileMenuPrim topMenu) subMenus
      return (menuButton,choose clickEvents)

-- | normalise decomposes the menu into a title plus a list of submenus.
normalise :: HTkMenu value -> (String,[MenuType.MenuPrim String value])
normalise (HTkMenu menuPrim) =
   case menuPrim of
      MenuType.Menu title subMenus -> (title,subMenus)
      Button s value -> (s,[menuPrim])
      Blank -> ("",[Blank])


-- | Set tearoff if we want tearoff menus, which means ones which open a
-- new top-level window.
tearoff :: Bool
tearoff = False

-- | Compiles the menu and inserts it into the parent (which is itself a menu),
-- returning the event click operation
compileMenuPrim :: Menu -> MenuType.MenuPrim String value -> IO (Event value)
compileMenuPrim parent menuPrim =
   case menuPrim of
      Button string value ->
         do
            menuCommand <- createMenuCommand parent [text string]
            event <- clicked menuCommand
            return (event >> return value)
      Blank ->
         do
            menuSeparator <- createMenuSeparator parent []
            return never
      MenuType.Menu title subMenuPrims ->
         do
            cascade <- createMenuCascade parent [text title]
            innerMenu <- createMenu parent tearoff []
            cascade # menu innerMenu
            events <- mapM (compileMenuPrim innerMenu) subMenuPrims
            return (choose events)

{- HTkMenu is a user-friendly interface to HTk's menu operations, which 
   compiles a version of MenuType.MenuPrim to an HTk menu. -}
module HTkMenu(
   HTkMenu(..),
   compileHTkMenu,
   ) where

import Computation

import Events

import Core(HasCommand(..))
import Configuration(HasText(..))

import Menu
import MenuCascade
import MenuSeparator
import MenuCommand

import MenuType 
   

-- ----------------------------------------------------------------------
-- The HTkMenu type
-- ----------------------------------------------------------------------

---
-- Describes a menu to be compiled.  
-- The value identifies the buttons in the menu so the client
--    can tell which was clicked.
-- The (Maybe String) is an optional title which is given to menu
--    cascades.
newtype HTkMenu value = HTkMenu (MenuPrim (Maybe String) value)

-- ----------------------------------------------------------------------
-- compileHTkMenu
-- ----------------------------------------------------------------------

---
-- compileHTkMenu compiles a menu and inserts it in the given parent (which
-- could be a window, for example).  It returns (1) the compiled menu; 
-- (2) an event which occurs when any of the items in the menu are clicked.
-- The menu is not actually inserted into the parent.  
--    parent # menu newMenu
-- (which can be used for parent widgets which have a special place for
-- the menu, such as the top bar) or else you put it into a menu button.
-- 
compileHTkMenu :: HasMenu parent => parent -> HTkMenu value 
   -> IO (Menu,Event value)
compileHTkMenu parent (HTkMenu menuPrim) =
   do
      topMenu <- createMenu parent tearoff []
      clickEvent <- compileMenuPrim topMenu menuPrim
      return (topMenu,clickEvent)


---
-- Set tearoff if we want tearoff menus, which means ones which open a
-- new top-level window.
tearoff :: Bool
tearoff = False

---
-- Compiles the menu and inserts it into the parent (which is itself a menu),
-- returning the event click operation
compileMenuPrim :: Menu -> MenuPrim (Maybe String) value
   -> IO (Event value)
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
      MenuType.Menu titleOpt subMenuPrims ->
         do
            cascade <- createMenuCascade parent
               (case titleOpt of
                  Nothing -> []
                  Just title -> [text title]
                  )
            innerMenu <- createMenu parent tearoff []
            cascade # menu innerMenu
            events <- mapM (compileMenuPrim innerMenu) subMenuPrims
            return (choose events)
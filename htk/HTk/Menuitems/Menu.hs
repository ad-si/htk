{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | HTk\'s /menus/.
-- A @Menu@ is a container for menu structures.
module HTk.Menuitems.Menu (

  Menu(..),
  HasMenu(..),

  createMenu,
  popup,

  post,
  unpost,

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Geometry
import Reactor.ReferenceVariables
import Events.Destructible
import Events.Synchronized
import Util.Computation
import HTk.Containers.Window


-- -----------------------------------------------------------------------
-- Menu
-- -----------------------------------------------------------------------

-- | The @Menu@ datatype.
data Menu = Menu GUIOBJECT (Ref Int)


-- -----------------------------------------------------------------------
-- class HasMenu
-- -----------------------------------------------------------------------

-- | Containers for menus (toplevel windows and menubuttons) instantiate the
-- @class HasMenu@.
class GUIObject w => HasMenu w where
  menu :: Menu -> Config w
  menu m w =
    do
      let (GUIOBJECT _ mostref) = toGUIObject m
      most <- getRef mostref
      cset w "menu" (show (objectname most))

-- | Windows are containers for menus.
instance (Window w, GUIObject w) => HasMenu w


-- -----------------------------------------------------------------------
-- Menu Creation Command
-- -----------------------------------------------------------------------

createMenu :: GUIObject par => par
   -- ^ tearoff.  If True, means menu will be displayed in a
   -- separate top-level window.
   -> Bool
   -> [Config Menu]
   -> IO Menu
createMenu par to ol =
  do
    w <- createGUIObject (toGUIObject par) MENU menuMethods
    r <- newRef (if to  then 1 else 0)
    configure (Menu w r) (tearOff (if to then On else Off)  : ol)


-- -----------------------------------------------------------------------
-- Popup Menu
-- -----------------------------------------------------------------------

-- | Posts a menu (e.g. in respose of a keystroke or mousebutton press).
popup :: GUIObject i => Menu
   -- ^ The menu to post.
   -> Position
   -- ^ The position to pop-up.
   -> Maybe i
   -- ^ An optional entry to activate when the menu pops-up.
   -> IO ()
   -- ^ None.
popup m pos@(x,y) ent@Nothing =
  execMethod m (\nm -> tkPopup nm x y "")
popup m pos@(x,y) ent@(Just entry) =
  do
    name <- getObjectName (toGUIObject entry)
    case name of
      ObjectName s -> execMethod m (\nm -> tkPopup nm x y s)
      MenuItemName _ i -> execMethod m (\nm -> tkPopup nm x y (show i))
      _ -> done

tkPopup :: ObjectName -> Distance -> Distance -> String -> TclScript
tkPopup wn x y ent = ["tk_popup " ++ show wn ++ " " ++
        show x ++ " " ++ show y ++ " " ++ ent]
{-# INLINE tkPopup #-}


-- -----------------------------------------------------------------------
-- menu instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq Menu where
  w1 == w2 = toGUIObject w1 == toGUIObject w2

-- | Internal.
instance GUIObject Menu where
  toGUIObject (Menu w _) = w
  cname _ = "Menu"

-- | A menu can be destroyed.
instance Destroyable Menu where
  -- Destroys a menu.
  destroy = destroy . toGUIObject

-- | A menu has standard widget properties
-- (concerning focus, cursor).
instance Widget Menu

-- | You can synchronize on a menu object.
instance Synchronized Menu where
  -- Synchronizes on a menu object.
  synchronize w = synchronize (toGUIObject w)

-- | A menu has a configureable border.
instance HasBorder Menu

-- | A menu has a normal foreground and background colour and an
-- active\/disabled foreground and background colour.
instance HasColour Menu where
  legalColourID w "background" = True
  legalColourID w "foreground" = True
  legalColourID w "activebackground" = True
  legalColourID w "activeforeground" = True
  legalColourID w _ = False

-- | You can specify the font of a menu.
instance HasFont Menu


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

-- | A tear-off entry can be displayed with a menu.
tearOff :: Toggle
   -- ^ @On@ if you wish to display a tear-off
   -- entry, otherwise @Off@.
   -> Config Menu
   -- ^ The conerned menu.
tearOff tg mn = cset mn "tearoff" tg


-- -----------------------------------------------------------------------
-- Posting and Unposting Menues
-- -----------------------------------------------------------------------

-- | Displays a menu at the specified position.
post :: Menu
   -- ^ the menu to post.
   -> Position
   -- ^ the position to post the menu at.
   -> IO ()
   -- ^ None.
post mn pos@(x, y) = execMethod mn (\name -> tkPost name x y)

-- | Unmaps the menu.
unpost :: Menu
   -- ^ the menu to unmap.
   -> IO ()
   -- ^ None.
unpost mn = execMethod mn (\name -> tkUnPost name)


-- -----------------------------------------------------------------------
-- Menu methods
-- -----------------------------------------------------------------------

menuMethods = defMethods{ createCmd = tkCreateMenu,
                          packCmd = packCmd voidMethods }


-- -----------------------------------------------------------------------
-- Unparsing of Menu Commands
-- -----------------------------------------------------------------------

tkCreateMenu :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                [ConfigOption] -> TclScript
tkCreateMenu _ _ nm oid cnf =
  ["menu " ++ show nm ++ " " ++ showConfigs cnf]

tkPost :: ObjectName -> Distance -> Distance -> TclScript
tkPost name @ (ObjectName _) x y = [show name ++ " post " ++ show x ++ " " ++ show y]
tkPost name @ (MenuItemName mn i) _ _ = [show mn ++ " postcascade " ++ (show i)]
tkPost _ _ _ = []
{-# INLINE tkPost #-}

tkUnPost :: ObjectName -> TclScript
tkUnPost (MenuItemName _ _) = []
tkUnPost name = [show name ++ " unpost "]
{-# INLINE tkUnPost #-}

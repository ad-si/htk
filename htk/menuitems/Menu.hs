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
-- HTk's <strong>menus</strong>.<br>
-- A <code>Menu</code> is a container for menu structures.
module Menu (

  Menu(..),
  HasMenu(..),

  createMenu,
  popup,

  post,
  unpost,
        
) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Geometry
import Image
import BitMap
import ReferenceVariables
import Destructible
import Synchronized
import Computation
import Events
import Window


-- -----------------------------------------------------------------------
-- Menu
-- -----------------------------------------------------------------------

---
-- The <code>Menu</code> datatype.
data Menu = Menu GUIOBJECT (Ref Int)


-- -----------------------------------------------------------------------
-- class HasMenu
-- -----------------------------------------------------------------------

---
-- Containers for menus (toplevel windows and menubuttons) instantiate the
-- <code>class HasMenu</code>.
class GUIObject w => HasMenu w where
  menu :: Menu -> Config w
  menu m w =
    do
      let (GUIOBJECT _ mostref) = toGUIObject m
      most <- getRef mostref
      cset w "menu" (show (objectname most))

---
-- Windows are containers for menus.
instance Window w => HasMenu w


-- -----------------------------------------------------------------------
-- Menu Creation Command
-- -----------------------------------------------------------------------

createMenu :: GUIObject par => par -> Bool -> [Config Menu] -> IO Menu
createMenu par to ol =
  do
    w <- createGUIObject (toGUIObject par) MENU menuMethods
    r <- newRef (if to  then 1 else 0)
    configure (Menu w r) (tearOff (if to then On else Off)  : ol)


-- -----------------------------------------------------------------------
-- Popup Menu
-- -----------------------------------------------------------------------

---
-- Posts a menu (e.g. in respose of a keystroke or mousebutton press).
-- @param m       - The menu to post.
-- @param pos     - The position to pop-up.
-- @param ent     - An optional entry to activate when the menu pops-up.
-- @return result - None.
popup :: GUIObject i => Menu -> Position -> Maybe i -> IO ()
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

---
-- Internal.
instance Eq Menu where 
---
-- Internal.
  w1 == w2 = toGUIObject w1 == toGUIObject w2

---
-- Internal.
instance GUIObject Menu where 
---
-- Internal.
  toGUIObject (Menu w _) = w
---
-- Internal.
  cname _ = "Menu"

---
-- A menu can be destroyed.
instance Destroyable Menu where
---
-- Destroys a menu.
  destroy = destroy . toGUIObject

---
-- A menu has standard widget properties
-- (concerning focus, cursor).
instance Widget Menu

---
-- You can synchronize on a menu object.
instance Synchronized Menu where
---
-- Synchronizes on a menu object.
  synchronize w = synchronize (toGUIObject w)

---
-- A menu has a configureable border.
instance HasBorder Menu

---
-- A menu has a normal foreground and background colour and an
-- active/disabled foreground and background colour.
instance HasColour Menu where
---
-- Internal.
  legalColourID w "background" = True
  legalColourID w "foreground" = True
  legalColourID w "activebackground" = True
  legalColourID w "activeforeground" = True
  legalColourID w _ = False

---
-- You can specify the font of a menu.
instance HasFont Menu


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

---
-- A tear-off entry can be displayed with a menu.
-- @param tg      - <code>On</code> if you wish to display a tear-off
--                  entry, otherwise <code>Off</code>.
-- @return result - The conerned menu.
tearOff :: Toggle -> Config Menu
tearOff tg mn = cset mn "tearoff" tg


-- -----------------------------------------------------------------------
-- Posting and Unposting Menues
-- -----------------------------------------------------------------------

---
-- Displays a menu at the specified position.
-- @param mn      - the menu to post.
-- @param pos     - the position to post the menu at.
-- @return result - None.
post :: Menu -> Position -> IO ()
post mn pos@(x, y) = execMethod mn (\name -> tkPost name x y)

---
-- Unmaps the menu.
-- @param mn      - the menu to unmap.
-- @return result - None.
unpost :: Menu -> IO ()
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

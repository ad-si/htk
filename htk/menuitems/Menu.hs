{- #######################################################################

MODULE        : Menu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Menu. 

TO BE DONE    : For the time being, tearoff must be set at creation time 
                and no later. The problem with tk is that the tearoff
                option changes the identities of the menu items. The
                implementation with variable tearoffs is therefore
                much more complicated as one would like!

   #################################################################### -}


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

data Menu = Menu GUIOBJECT (Ref Int)


-- -----------------------------------------------------------------------
-- class HasMenu
-- -----------------------------------------------------------------------

class GUIObject w => HasMenu w where
  menu :: Menu -> Config w
  menu m w =
    do
      let (GUIOBJECT _ mostref) = toGUIObject m
      most <- getRef mostref
      cset w "menu" (show (objectname most))

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

popup :: GUIObject i => Menu -> Position -> Maybe i -> IO ()
popup m (x,y) Nothing =
  execMethod m (\nm -> tkPopup nm x y "")
popup m (x,y) (Just entry) =
  do
    name <- getObjectName (toGUIObject entry)
    case name of
      ObjectName s -> execMethod m (\nm -> tkPopup nm x y s)
      MenuItemName _ i -> execMethod m (\nm -> tkPopup nm x y (show i))
-- this is from Einar's implementation, I don't know if it makes sense
      _ -> done

tkPopup :: ObjectName -> Distance -> Distance -> String -> TclScript 
tkPopup wn x y ent = ["tk_popup " ++ show wn ++ " " ++ 
        show x ++ " " ++ show y ++ " " ++ ent]
{-# INLINE tkPopup #-}


-- -----------------------------------------------------------------------
-- menu instances
-- -----------------------------------------------------------------------

instance Eq Menu where 
  w1 == w2 = toGUIObject w1 == toGUIObject w2

instance GUIObject Menu where 
  toGUIObject (Menu w _) = w
  cname _ = "Menu"

instance Destroyable Menu where
  destroy   = destroy . toGUIObject

instance Widget Menu

instance Synchronized Menu where
  synchronize w = synchronize (toGUIObject w)

instance HasBorder Menu

instance HasColour Menu where
  legalColourID w "background" = True
  legalColourID w "foreground" = True
  legalColourID w "activebackground" = True
  legalColourID w "activeforeground" = True
  legalColourID w _ = False

instance HasFont Menu


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

tearOff :: Toggle -> Config Menu
tearOff tg mn = cset mn "tearoff" tg


-- -----------------------------------------------------------------------
-- Posting and Unposting Menues
-- -----------------------------------------------------------------------

post :: Menu -> Position -> IO ()
post mn (x, y) = execMethod mn (\name -> tkPost name x y)

unpost :: Menu -> IO ()
unpost mn = execMethod mn (\name -> tkUnPost name)


-- -----------------------------------------------------------------------
-- Menu methods
-- -----------------------------------------------------------------------

menuMethods = defMethods{ createCmd = tkCreateMenu,
                          packCmd = packCmd voidMethods }

{-
-- -----------------------------------------------------------------------
-- Menu packing
-- -----------------------------------------------------------------------

addMenuItem :: Menu -> GUIOBJECT -> Methods -> IO (Event a) -> IO ()
addMenuItem mn@(Menu w pv) item meths cmd =
  do
    packMenuItem (toGUIObject mn) item (Just meths);
        c
        } 
-}

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

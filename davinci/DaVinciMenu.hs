{- #########################################################################

MODULE        : DaVinciMenu
AUTHOR        : Carla Blanck Purper,
                Einar W. Karlsen 
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : Encapsulation of DaVinci Menus.

TO BE DONE    : Accelerators

                File menu events are always disabled. Their enabling
                conflicts with the enabling of application menus.

                We do not associate menus with individual objects. 
                This must be done on the type level!!!


   ######################################################################### -}


module DaVinciMenu (

   Graph,
   
   Menu,
   installGraphMenu,
   installNodeTypeMenu,
   installEdgeTypeMenu
   
      
   ) where

import SIM
import GUICore
import HTk
import Menu
import MenuItem
import Button
import GUIIntrinsics

import DaVinciGraphTerm
import DaVinciCore
import DaVinciEvent

import Debug(debug)


-- ---------------------------------------------------------------------------
-- Install Menu
-- ---------------------------------------------------------------------------

installGraphMenu :: Menu a -> Graph -> IO ()
installGraphMenu mn g = do {
        gl <- getChildObjects (toGUIObject mn);
        it <- sequence (map toMenuType gl);
        withGraph g (return (createMenus it));
        withGraph g (return (activateMenus it));
        done
        }


installEdgeTypeMenu :: Menu a -> Graph -> TypeId -> [AttrAssoc] -> IO ()
installEdgeTypeMenu mn g tid assocs = do {
        gl <- getChildObjects (toGUIObject mn);
        it <- sequence (map toMenuType gl);
        withGraph g (return (
                "visual(add_rules([er("++show tid++",[m("++show it ++")," ++
                 drop 1 (show assocs)   ++ ")]))"
                ));
        done
        }

installNodeTypeMenu :: Menu a -> Graph -> TypeId -> [AttrAssoc] -> IO ()
installNodeTypeMenu mn g tid assocs = do {
        gl <- getChildObjects (toGUIObject mn);
        it <- sequence (map toMenuType gl);
        withGraph g (return (
                "visual(add_rules([nr("++show tid++",[m("++show it++")," ++
                drop 1 (show assocs) ++         ")]))"
                ));
        done
        }



-- ---------------------------------------------------------------------------
-- Generate DaVinci Menu Definition
-- ---------------------------------------------------------------------------

toMenuType :: GUIOBJECT -> IO MenuType
toMenuType guio = do {
        t <- getText guio;
        acc <-  getAccelerator guio; -- unused
        mne <-  getUnderline guio;  -- unused
        kind <- getObjectKind guio;
        makeMenu guio kind (show (objectID guio)) t acc mne 
        }

makeMenu :: GUIOBJECT -> ObjectKind -> String -> 
                           String -> String -> Int -> IO MenuType
makeMenu guio CLICKBUTTON oid t acc mne = 
        return (MenuType oid t Nothing Nothing)
makeMenu guio SEPARATOR oid t acc mne = 
        return Blank
makeMenu guio MENUBUTTON oid t acc mne = do {
        chs <- getChildObjects (toGUIObject guio);
        case chs of
                [] -> return Blank
                [mn] -> do {
                        gl <- getChildObjects (toGUIObject mn);
                        it <- sequence (map toMenuType gl);
                        return (SubMenuType oid t it Nothing)
                        }
        }


-- ---------------------------------------------------------------------------
-- DaVinci commands for activating and creating menus.
-- (Only used for Graph menus.)
-- ---------------------------------------------------------------------------

activateMenus :: [MenuType] -> String
-- This activates all the menus.  It's only necessary for Graph's.
activateMenus menus = 
   callDaVinci "app_menu" [
      callDaVinci "activate_menus" [
         show (getMenuIds menus)
         ]]
        
getMenuIds :: [MenuType] -> [MenuId]
getMenuIds [] = []
getMenuIds ((MenuType menuId _ _ _):rest) = menuId : getMenuIds rest
getMenuIds ((SubMenuType menuId _ menus _):rest) = 
   menuId : (getMenuIds menus ++ getMenuIds rest)
getMenuIds (Blank : rest) = getMenuIds rest

createMenus :: [MenuType] -> String
createMenus x =
   "("++ 
      (callDaVinci "app_menu" [show x]) 
      ++  ")"

-- ---------------------------------------------------------------------------
-- DaVinci Menu Definitions
-- ---------------------------------------------------------------------------

data MenuType = 
      MenuType MenuId MenuLabel MenuMne MenuAcc
      -- a single button. 
   |  SubMenuType MenuId MenuLabel [MenuType] MenuMne
      -- a submenu
   |  Blank
      -- a gap (left on a menu to separate menu items)

type MenuId     =  String
-- For MenuType the MenuId is the string sent by daVinci when the menu 
-- button is pressed.  For both MenuType and SubMenuType, the MenuId
-- can be used to disable the item.

type MenuLabel    = String
-- The MenuLabel is the string displayed to the user on the menu.

-- For MenuType MenuMod and MenuMod (if set, and they should both be
-- set if either is set) give a keyboard shortcut.

data MenuMod    = NoMenuMod | MenuMod Modifier
-- A Modifier is described in htk/resources/GUIEvent.hs.  It is Control,
-- Shift, Meta or Alt.  It is the key to be pressed with a MenuAcc
-- to shortcut the menu.
-- Unused.

type MenuAcc    = Maybe (MenuMod,String)
-- The String in MenuAcc should have precisely one character.
-- This is always Nothing

type MenuMne    = Maybe String
-- This is a "Motif mnemonic" whatever that is.  It's always Nothing.


-- ---------------------------------------------------------------------------
-- Unparsing of DaVinci Menu Definitions
-- ---------------------------------------------------------------------------

instance Show MenuType where
   showsPrec d  (MenuType menuId menuLabel Nothing Nothing) =
      callDaVinciAcc "menu_entry" [show menuId,show menuLabel]
   showsPrec d  (MenuType menuId menuLabel (Just mne) (Just (mmod,macc))) =
      callDaVinciAcc "menu_entry_mne" 
         [show menuId,show menuLabel,show mne,show mmod,show macc]
   showsPrec d (SubMenuType menuId menuLabel mtypes Nothing) =
      callDaVinciAcc "submenu_entry"
         [show menuId,show menuLabel,show mtypes]
   showsPrec d (SubMenuType menuId menuLabel mtypes (Just mne)) =
      callDaVinciAcc "submenu_entry_mne"
         [show menuId,show menuLabel,show mtypes,show mne]
   showsPrec d Blank = ("blank"++)

-- callDaVinci and callDaVinciAcc unparses the standard DaVinci function call.
-- callDaVinciAcc allows you to add a parameter to append at the end.
callDaVinci :: String -> [String] -> String
callDaVinci funName funArgs = callDaVinciAcc funName funArgs ""

callDaVinciAcc :: String -> [String] -> String -> String
callDaVinciAcc funName funArgs toAppend =
   let
      withCommas [] = ""
      withCommas [one] = one
      withCommas (first:rest) = first ++ "," ++ withCommas rest
   in
      funName ++ "(" ++ withCommas funArgs ++ ")" ++ toAppend

instance Show MenuMod where
   showsPrec d  (MenuMod Alt)  r           = "alt" ++ r
   showsPrec d  (MenuMod Shift)  r         = "shift" ++ r
   showsPrec d  (MenuMod Control) r        = "control" ++ r
   showsPrec d  (MenuMod Meta) r           = "meta" ++ r
   showsPrec d  NoMenuMod  r               = "none" ++ r


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
installGraphMenu menu graph = 
    do
       menuParts <- getChildObjects (toGUIObject menu)
       menuTypes <- sequence (map toMenuType menuParts)
       withGraph graph (return (createMenus menuTypes))
       withGraph graph (return (activateMenus menuTypes))
       done

-- For specifications of the arguments see installNodeOrEdgeTypeMenu
installNodeTypeMenu :: Menu a -> Graph -> TypeId -> [AttrAssoc] -> IO ()
installNodeTypeMenu = installNodeOrEdgeTypeMenu "nr"

installEdgeTypeMenu :: Menu a -> Graph -> TypeId -> [AttrAssoc] -> IO ()
installEdgeTypeMenu = installNodeOrEdgeTypeMenu "er" 

installNodeOrEdgeTypeMenu :: 
   String -> Menu a -> Graph -> TypeId -> [AttrAssoc] -> IO()
-- installNodeOrEdgeMenu is used for installing a new menu for
-- a DaVinci node or edge type.
-- Arguments:
--    1. Should be "er" for an EdgeType and "nr" for a NodeType.
--    2. The Menu
--    3. The Graph
--    4. The TypeId for the Node or Edge type in question.
--    5. AttrAssoc settings.
installNodeOrEdgeTypeMenu nodeOrEdge menu graph typeId assocs =
   do
      menuParts <- getChildObjects (toGUIObject menu)
      menuTypes <- sequence (map toMenuType menuParts)
      withGraph graph (return (
         callDaVinci "visual" [
            innerCallDaVinci "add_rules" [
               Arg [
                  CallDaVinci nodeOrEdge [
                     Arg typeId,
                     Arg ((
                        innerCallDaVinci "m" [
                           Arg menuTypes
                           ]
                        ) :
                        (map Arg assocs)
                        )
                     ]
                  ]
               ]
            ]
         ))
      done

{- Typical output (an edge type with an empty assocs list):

   visual(add_rules([er("255",[m([menu_entry("254","ArcType1")])])]))
   -}

-- ---------------------------------------------------------------------------
-- Generate DaVinci Menu Definition
-- ---------------------------------------------------------------------------

toMenuType :: GUIOBJECT -> IO MenuType
toMenuType guiObject = 
   do
      text <- getText guiObject
      kind <- getObjectKind guiObject
      makeMenu guiObject kind (show (objectID guiObject)) text 

makeMenu :: GUIOBJECT -> ObjectKind -> String -> String  -> IO MenuType
makeMenu guiObject CLICKBUTTON objectId text = 
   return (MenuType objectId text Nothing Nothing)
makeMenu guiObject SEPARATOR objectId text = 
   return Blank
makeMenu guiObject MENUBUTTON objectId text = 
   do
      children <- getChildObjects (toGUIObject guiObject)
      case children of
         [] -> return Blank
         [menu] -> 
            do
               menuParts <- getChildObjects (toGUIObject menu)
               menuTypes <- sequence (map toMenuType menuParts)
               return (SubMenuType objectId text menuTypes Nothing)

-- ---------------------------------------------------------------------------
-- DaVinci commands for activating and creating menus.
-- (Only used for Graph menus.)
-- ---------------------------------------------------------------------------

activateMenus :: [MenuType] -> String
-- This activates all the menus.  It's only necessary for Graph's.
activateMenus menus = 
   callDaVinci "app_menu" [
      innerCallDaVinci "activate_menus" [
         Arg (getMenuIds menus)
         ]]
        
getMenuIds :: [MenuType] -> [MenuId]
getMenuIds [] = []
getMenuIds ((MenuType menuId _ _ _):rest) = menuId : getMenuIds rest
getMenuIds ((SubMenuType menuId _ menus _):rest) = 
   menuId : (getMenuIds menus ++ getMenuIds rest)
getMenuIds (Blank : rest) = getMenuIds rest

createMenus :: [MenuType] -> String
createMenus x =
   callDaVinci "app_menu" [innerCallDaVinci "create_menus" [Arg x]]

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
      callDaVinciAcc "menu_entry" [Arg menuId,Arg menuLabel]
   showsPrec d  (MenuType menuId menuLabel (Just mne) (Just (mmod,macc))) =
      callDaVinciAcc "menu_entry_mne" 
         [Arg menuId,Arg menuLabel,Arg mne,Arg mmod,Arg macc]
   showsPrec d (SubMenuType menuId menuLabel mtypes Nothing) =
      callDaVinciAcc "submenu_entry"
         [Arg menuId,Arg menuLabel,Arg mtypes]
   showsPrec d (SubMenuType menuId menuLabel mtypes (Just mne)) =
      callDaVinciAcc "submenu_entry_mne"
         [Arg menuId,Arg menuLabel,Arg mtypes,Arg mne]
   showsPrec d Blank = ("blank"++)

instance Show MenuMod where
   showsPrec d  (MenuMod Alt)  r           = "alt" ++ r
   showsPrec d  (MenuMod Shift)  r         = "shift" ++ r
   showsPrec d  (MenuMod Control) r        = "control" ++ r
   showsPrec d  (MenuMod Meta) r           = "meta" ++ r
   showsPrec d  NoMenuMod  r               = "none" ++ r







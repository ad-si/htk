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
        acc <-  getAccelerator guio;
        mne <-  getUnderline guio;
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
-- The Correwsponding DaVinci Commands
-- ---------------------------------------------------------------------------

activateMenus :: [MenuType] -> String
activateMenus mt = "app_menu(activate_menus("++ show (getMenuIds mt) ++"))"
        
getMenuIds :: [MenuType] -> [MenuId]
getMenuIds [] = []
getMenuIds ((MenuType oid _ _ _):l) = oid : getMenuIds l
getMenuIds ((SubMenuType oid _ tl _):l) = oid : (getMenuIds tl ++ getMenuIds l)
getMenuIds (Blank : l) = getMenuIds l

createMenus :: [MenuType] -> String
createMenus x = ("app_menu(create_menus(" ++ show x ++ "))")

-- ---------------------------------------------------------------------------
-- DaVinci Menu Definitions
-- ---------------------------------------------------------------------------

data MenuType = 
          MenuType MenuId MenuLbl MenuMne MenuAcc
        | SubMenuType MenuId MenuLbl [MenuType] MenuMne
        | Blank

type MenuId     =  String

data MenuMod    = NoMenuMod | MenuMod Modifier                  

type MenuAcc    = Maybe (MenuMod,String)

type MenuLbl    = String

type MenuMne    = Maybe String

-- ---------------------------------------------------------------------------
-- Unparsing of DaVinci Menu Definitions
-- ---------------------------------------------------------------------------

instance Show MenuType where
        showsPrec d  (MenuType mid mlbl Nothing Nothing) r =
                "menu_entry("++ show mid++comma++ show mlbl++")" 
                ++ r
        showsPrec d  (MenuType mid mlbl (Just mne) (Just (mmod,macc))) r=
                "menu_entry_mne(" ++ show mid++comma++ show mlbl ++comma++
                show mne ++comma++ show mmod ++comma++ show macc ++")"
                ++ r
        showsPrec d (SubMenuType mid mlbl mtypes Nothing) r =
                "submenu_entry(" ++ show mid++comma++ show mlbl ++comma++
                show mtypes ++")"
                ++ r    
        showsPrec d (SubMenuType mid mlbl mtypes (Just mne)) r =
                "submenu_entry_mne(" ++ show mid++comma++
                show mlbl ++comma++ show mtypes ++ comma ++ show mne    ++")" 
                ++ r
        showsPrec d Blank r = "blank" ++ r


instance Show MenuMod where
        showsPrec d  (MenuMod Alt)  r           = "alt" ++ r
        showsPrec d  (MenuMod Shift)  r         = "shift" ++ r
        showsPrec d  (MenuMod Control) r        = "control" ++ r
        showsPrec d  (MenuMod Meta) r           = "meta" ++ r
        showsPrec d  NoMenuMod  r               = "none" ++ r


comma      = "," 

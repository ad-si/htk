{- #########################################################################

   This module implements primitive events, handling the installation
   of items of type
      GlobalMenu
         (for global events),
      LocalMenu NodeId
         (for node events)
      LocalMenu EdgeId
         (for edge events)
   Hence the latter two are not quite the user-level menu installation
   commands, since they take NodeId/EdgeId parameters rather than
   Nodes or Edges.  


   ######################################################################### -}

module DaVinciMenu (

   Graph,

   configGraphMenu,    -- :: GlobalMenu -> Graph -> IO Graph
   installNodeTypeMenu, -- :: MenuPrim (Maybe String) (NodeId -> IO()) 
                        --       -> Graph -> TypeId -> [AttrAssoc] -> IO ()
   installEdgeTypeMenu  -- :: MenuPrim (Maybe String) (EdgeId -> IO())
                        --       -> Graph -> TypeId -> [AttrAssoc] -> IO ()

   ) where

import Char
import IO

import Computation(done)
import Debug(debug)

import GraphDisp(MenuPrim(..),mapMMenuPrim,mapMMenuPrim',
   GlobalMenu(..),LocalMenu(..))

import DaVinciGraphTerm
import DaVinciActions
import DaVinciCore

-- ---------------------------------------------------------------------------
-- Install Menu
-- ---------------------------------------------------------------------------

configGraphMenu :: GlobalMenu -> Graph -> IO Graph
configGraphMenu (GlobalMenu menuPrim) graph = 
    do
       compiledMenu <- makeGraphMenu menuPrim graph
       withGraph graph (return (createMenus [compiledMenu]))
       withGraph graph (return (activateMenus [compiledMenu]))
       return graph

-- For specifications of the arguments see installNodeOrEdgeTypeMenu
installNodeTypeMenu :: LocalMenu NodeId -> Graph -> TypeId -> [AttrAssoc]
   -> IO ()
installNodeTypeMenu (LocalMenu menuPrim) graph typeId assocs = 
   do
      compiledMenu <- makeNodeMenu menuPrim graph
      installNodeOrEdgeTypeMenu "nr" compiledMenu graph typeId assocs

installEdgeTypeMenu :: LocalMenu EdgeId -> Graph -> TypeId -> [AttrAssoc]
    -> IO ()
installEdgeTypeMenu (LocalMenu menu) graph typeId assocs = 
   do
      compiledMenu <- makeEdgeMenu menu graph
      installNodeOrEdgeTypeMenu "er" compiledMenu graph typeId assocs

installNodeOrEdgeTypeMenu :: 
   String -> MenuPrim (String,MenuItemId) MenuItemId
   -> Graph -> TypeId -> [AttrAssoc] -> IO()
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
      let
         menus = case menu of
            Menu (title,menuItemId) menus 
               | (all isSpace title) -> menus
            _ -> [menu]
      withGraph graph (return (
         callDaVinci "visual" [
            innerCallDaVinci "add_rules" [
               Arg [
                  CallDaVinci nodeOrEdge [
                     Arg typeId,
                     Arg ((
                        innerCallDaVinci "m" [
                           Arg menus
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

makeGraphMenu :: (MenuPrim (Maybe String) (IO())) -> Graph
   -> IO (MenuPrim (String,MenuItemId) MenuItemId)
makeGraphMenu menu graph =
   makeGeneralMenu
      (\ action -> newGlobalEvent (fRegister graph) action)
      menu
      graph

makeNodeMenu :: (MenuPrim (Maybe String) (NodeId -> IO())) -> Graph
   -> IO (MenuPrim (String,MenuItemId) MenuItemId)
makeNodeMenu menu graph =
   makeGeneralMenu 
      (\ action -> newNodeEvent (fRegister graph) action)
      menu
      graph

makeEdgeMenu :: (MenuPrim (Maybe String) (EdgeId -> IO())) -> Graph 
   -> IO (MenuPrim (String,MenuItemId) MenuItemId)
makeEdgeMenu menu graph =
   makeGeneralMenu
      (\ action -> newEdgeEvent (fRegister graph) action)
      menu
      graph

makeGeneralMenu :: (menuAction -> IO MenuItemId)
   -> (MenuPrim (Maybe String) menuAction) -> Graph 
   -> IO (MenuPrim (String,MenuItemId) MenuItemId) 
makeGeneralMenu registerAct menu0 graph =
   do
      (menu1 :: MenuPrim (Maybe String) MenuItemId) <-
         mapMMenuPrim registerAct menu0
      (menu2 :: MenuPrim (String,MenuItemId) MenuItemId) <-
         mapMMenuPrim' 
            (\ stringOpt ->
               do
                  let
                     title = case stringOpt of
                        Nothing -> " "
                        Just title -> title
                  menuItemId <- newGlobalEvent (fRegister graph)
                     (ioError(userError(
                        "DaVinciMenu: daVinci returned event for submenu id??"
                        )))
                  return (title,menuItemId)
               ) 
            menu1
      return menu2  

-- ---------------------------------------------------------------------------
-- DaVinci commands for activating and creating menus.
-- (Only used for Graph menus.)
-- ---------------------------------------------------------------------------

activateMenus :: [MenuType] -> String
-- This activates all the menus.  It's only necessary for Graph's.
activateMenus menus = 
   callDaVinci "app_menu" [
      innerCallDaVinci "activate_menus" [
         Arg (getMenuItemIds menus)
         ]]
        
getMenuItemIds :: [MenuType] -> [MenuItemId]
getMenuItemIds [] = []
getMenuItemIds ((Button _ menuItemId):rest) = menuItemId : getMenuItemIds rest
getMenuItemIds ((Menu (_,menuItemId) menus):rest) = 
   menuItemId : (getMenuItemIds menus ++ getMenuItemIds rest)
getMenuItemIds (Blank:rest) = getMenuItemIds rest

createMenus :: [MenuType] -> String
createMenus x =
   callDaVinci "app_menu" [innerCallDaVinci "create_menus" [Arg x]]

-- ---------------------------------------------------------------------------
-- Compiling a Menu into DaVinci form.
-- ---------------------------------------------------------------------------

-- MenuType represents a menu in which the menu items have been 
-- allocated MenuItemId's.
type MenuType = MenuPrim (String,MenuItemId) MenuItemId

instance Show MenuType where
   showsPrec _ (Button menuLabel menuItemId) =
      callDaVinciAcc "menu_entry" [Arg menuItemId,Arg menuLabel]
   showsPrec _ Blank = ("blank"++)
   showsPrec _ (Menu (menuLabel,menuItemId) menus) =
      callDaVinciAcc "submenu_entry" [Arg menuItemId,Arg menuLabel,Arg menus]







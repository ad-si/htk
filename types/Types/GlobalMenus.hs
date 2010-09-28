-- | This module defines various global menus (new object types, new
-- display types and so on) which are used in constructing display
-- types.
module Types.GlobalMenus(
   newDefaultMenu,
   newObjectTypeMenu,
   newDisplayTypeMenu,
   openDisplayType,
   ) where

import Maybe

import HTk.Toplevel.HTk
import HTk.Toolkit.MenuType as Menu
import HTk.Toolkit.HTkMenu

import Graphs.GraphDisp
import Graphs.GraphConfigure

import Types.ViewType
import Types.ObjectTypes
import Types.DisplayTypes
import {-# SOURCE #-} Types.DisplayView
import {-# SOURCE #-} Types.VersionGraph


-- | newDefaultMenu is a recommended general-purpose menu for attaching
-- to view displays
newDefaultMenu ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> View -> IO GlobalMenu
newDefaultMenu displaySort view =
   do
      objectTypeMenu <- newObjectTypeMenu view
      displayTypeMenu <- newDisplayTypeMenu view
      let
         commitViewAct = commitViewInGraph view
         commitViewMenu = GlobalMenu (Button "Commit" commitViewAct)

         openDisplayTypeAct =
            do
               openDisplayType displaySort view
               done
         openDisplayTypeMenu =
            GlobalMenu (Button "Open Display" openDisplayTypeAct)
      return (combineGlobalMenus
         [commitViewMenu,objectTypeMenu,displayTypeMenu,openDisplayTypeMenu])

newObjectTypeMenu :: View -> IO GlobalMenu
newObjectTypeMenu view =
   do
      wrappedObjectTypeTypes <- getAllObjectTypeTypes
      let
         menuItem :: WrappedObjectTypeTypeData -> Maybe (String,IO ())
         menuItem (WrappedObjectTypeTypeData objectType) =
            fmap
               (\ (label,mkAction) -> (label,mkAction view))
               (createObjectTypeMenuItemPrim objectType)

         menuItems :: [(String,IO ())]
         menuItems = catMaybes (map menuItem wrappedObjectTypeTypes)

         menu :: MenuPrim (Maybe String) (IO ())
         menu = Menu.Menu (Just "Create Object Type")
            (map (\ (label,action) -> Button label action) menuItems)

         globalMenu :: GlobalMenu
         globalMenu = GlobalMenu menu

      return globalMenu

newDisplayTypeMenu :: View -> IO GlobalMenu
newDisplayTypeMenu view =
   do
      wrappedDisplayTypeTypes <- getAllDisplayTypeTypes
      let
         menuItem :: WrappedDisplayType -> Maybe (String,IO ())
         menuItem (WrappedDisplayType displayType) =
            fmap
               (\ (label,mkAction) -> (label,mkAction view))
               (createDisplayTypeMenuItemPrim displayType)

         menuItems :: [(String,IO ())]
         menuItems = catMaybes (map menuItem wrappedDisplayTypeTypes)

         menu :: MenuPrim (Maybe String) (IO ())
         menu = Menu.Menu (Just "Create Display Type")
            (map (\ (label,action) -> Button label action) menuItems)

         globalMenu :: GlobalMenu
         globalMenu = GlobalMenu menu

      return globalMenu

-- | This action opens a new display and returns it
openDisplayType ::
   GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms
   => (Graph graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   -> View
   -> IO (Maybe (DisplayedView graph graphParms node nodeType
      nodeTypeParms arc arcType arcTypeParms))
openDisplayType graphSort view =
   do
      -- (1) get all display types
      wrappedDisplayTypes <- getAllDisplayTypes view

      -- (2) construct list suitable for menu type.
      let
         displayTypeMenuItemsOpt =
            map
               (\ (WrappedDisplayType displayType) ->
                  fmap
                     (\ (name,actionFn) -> (name,actionFn view))
                     (openDisplayMenuItemPrim graphSort displayType)
                  )
               wrappedDisplayTypes

         displayTypeMenuItems = catMaybes displayTypeMenuItemsOpt

         -- (3) construct an HTkMenu item
         menu = HTkMenu (Menu.Menu "Select DisplayType"
            (map (\ (str,action) -> Button str action) displayTypeMenuItems))

      -- (4) query it
      top <- createToplevel []
      (menuButton,actionEvent) <- compileHTkMenu top menu
      pack menuButton []

      -- (5) provide a cancel button
      cancelButton <- newButton top [text "Cancel"]
      pack cancelButton []
      cancelled <- clicked cancelButton

      -- (6) wait for a result
      requestedActionOpt <- sync (
            cancelled >>> return Nothing
         +> actionEvent >>>=
            (\ requestedAction -> return (Just requestedAction))
         )

      -- (7) destroy window
      destroy top

      -- (8) do construction
      case requestedActionOpt of
         Nothing -> return Nothing
         Just requestedAction -> requestedAction

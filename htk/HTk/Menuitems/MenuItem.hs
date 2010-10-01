-- | The @module MenuItem@ exports general resources for menu
-- items.
module HTk.Menuitems.MenuItem (

  MenuItem,
  createMenuItem,
  menuItemMethods,

  HasColour(..),
  HasPhoto(..),

  SelectButton(..),
  ToggleButton(..),
  HasAccelerator(..),

  buttonColours

) where

import HTk.Kernel.Core
import HTk.Kernel.ButtonWidget
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Components.Image
import Reactor.ReferenceVariables
import Util.Computation
import Events.Events
import HTk.Menuitems.Menu


-- -----------------------------------------------------------------------
-- class MenuContainer
-- -----------------------------------------------------------------------

-- | Menu items instantiate the abstract @class MenuItem@.
class GUIObject w => MenuItem w


-- -----------------------------------------------------------------------
-- SelectButton
-- -----------------------------------------------------------------------

-- | A select button can be selected or not selected.
class ButtonWidget w => SelectButton w where
  -- Sets the selection state of the select button.
  selectionState    :: Toggle -> Config w
  -- Gets the selection state of the select button
  getSelectionState :: w -> IO Toggle
  -- Returns an event for selection actions.
  selectionStateSet :: w -> Event Toggle

  selectionState On w =
    execMethod (toGUIObject w) (\ nm -> tkSelect nm) >> return w

  selectionState Off w =
    execMethod (toGUIObject w) (\ nm -> tkDeselect nm) >> return w


-- -----------------------------------------------------------------------
-- Accelerator
-- -----------------------------------------------------------------------

-- | Menu items can have an optional text to display as a reminder
-- about a keystroke binding.
class GUIObject w => HasAccelerator w where
  -- Sets the accelerator text.
  accelerator    :: String -> Config w
  -- Gets the accelerator text.
  getAccelerator :: w -> IO String
  accelerator s w = cset w "accelerator" s
  getAccelerator w = cget w "accelerator"


-- -----------------------------------------------------------------------
-- Toggle buttons
-- -----------------------------------------------------------------------

-- | The state of a @ToggleButton@ can be toggled.
class SelectButton w => ToggleButton w where
  -- Toggles the state of a toggle button.
  toggleButton   :: w -> IO ()
  toggleButton w =
    execMethod (toGUIObject w) (\ nm -> tkToggle nm)


-- -----------------------------------------------------------------------
--  Unparsing of Button Commands
-- -----------------------------------------------------------------------

tkSelect :: ObjectName -> TclScript
tkSelect (MenuItemName name i) = []
tkSelect name = [show name ++ " select"]
{-# INLINE tkSelect #-}

tkDeselect :: ObjectName -> TclScript
tkDeselect (MenuItemName name i) = []
tkDeselect name = [show name ++ " deselect"]
{-# INLINE tkDeselect #-}

tkToggle :: ObjectName -> TclScript
tkToggle (MenuItemName name i) = []
tkToggle name = [show name ++ " toggle"]
{-# INLINE tkToggle #-}

tkButtonCmd :: ObjectID -> TclCmd
tkButtonCmd key = "Clicked " ++ show key
{-# INLINE tkButtonCmd #-}


-- -----------------------------------------------------------------------
-- MenuItem creation
-- -----------------------------------------------------------------------

-- | Internal.
createMenuItem :: MenuItem w => Menu -> MenuItemKind ->
                                (GUIOBJECT -> w) -> [Config w] -> IO w
createMenuItem menu@(Menu _ r) kind wrap ol =
  do
    i <- getRef r
    setRef r (i + 1)
    w <- createGUIObject (toGUIObject menu) (MENUITEM kind i)
                         menuItemMethods
    let mi = wrap w
    configure mi ol


-- -----------------------------------------------------------------------
-- item methods
-- -----------------------------------------------------------------------

-- | Internal.
menuItemMethods :: Methods
menuItemMethods = Methods tkGetMenuItemConfig
                          tkSetMenuItemConfigs
                          tkCreateMenuItem
                          (packCmd voidMethods)
                          (gridCmd voidMethods)
                          (destroyCmd voidMethods)
                          (bindCmd voidMethods)
                          (unbindCmd voidMethods)
                          (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- unparsing of menu commands
-- -----------------------------------------------------------------------

tkCreateMenuItem :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                    [ConfigOption] -> TclScript
tkCreateMenuItem nm kind _ {-nm-} _ args = tkCreateMenuItem' kind nm args'
  where args' = filter (not . isIllegalMenuItemConfig . first) args

tkCreateMenuItem' :: ObjectKind -> ObjectName -> [ConfigOption] ->
                     TclScript
tkCreateMenuItem' kind menu opts =
  [show menu ++ " add " ++ (show kind) ++ " " ++ (showECO opts)]

tkGetMenuItemConfig :: ObjectName -> ConfigID -> TclScript
tkGetMenuItemConfig (MenuItemName name i) "text" =
  [(show name) ++ " entrycget " ++ (show i) ++ " -label"]
tkGetMenuItemConfig (MenuItemName name i) cid
  | (isIllegalMenuItemConfig cid ) = []
tkGetMenuItemConfig (MenuItemName name i) cid =
  [show name ++ " entrycget " ++ show i ++ " -" ++ cid]
tkGetMenuItemConfig _ _ = []

tkSetMenuItemConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetMenuItemConfigs (MenuItemName name i) args =
  [show name ++ " entryconfigure " ++ (show i) ++ " " ++ showECO args]
tkSetMenuItemConfigs _ _ = []

showECO :: [ConfigOption] -> String
showECO [] = ""
showECO (("text",v) : ecl) = showConfig ("label", v) ++ " " ++ showECO ecl
showECO (x : ecl) =  showConfig x ++ " " ++ showECO ecl

first (a, b) = a


-- -----------------------------------------------------------------------
-- filtering of configs
-- -----------------------------------------------------------------------

isIllegalMenuItemConfig :: ConfigID -> Bool
isIllegalMenuItemConfig "indicatoron" = True
isIllegalMenuItemConfig "disabledforeground" = True
isIllegalMenuItemConfig "borderwidth" = True
isIllegalMenuItemConfig "relief" = True
isIllegalMenuItemConfig "cursor" = True
isIllegalMenuItemConfig "takefocus" = True
isIllegalMenuItemConfig "highlightbackground" = True
isIllegalMenuItemConfig "highlightcolor" = True
isIllegalMenuItemConfig "highlightthickness" = True
isIllegalMenuItemConfig "width" = True
isIllegalMenuItemConfig "height" = True
isIllegalMenuItemConfig "wraplength" = True
isIllegalMenuItemConfig "anchor" = True
isIllegalMenuItemConfig "justify" = True
isIllegalMenuItemConfig _ = False

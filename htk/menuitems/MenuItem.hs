{- #########################################################################

MODULE        : MenuItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Menu items. 

TO BE DONE    : yposition command
                activate command

   ######################################################################### -}


module MenuItem (
        ButtonWidget(..),

        HasColour(..),
        HasPhoto(..),

        SelectButton(..),
        ToggleButton(..),
        HasAccelerator(..),

        buttonColours,
        buttonMethods,
        activeEntryMethods,
        passiveEntryMethods             
        ) where


import Concurrency
import GUICore

import EventLoop
import Image
import BitMap
import Packer
import Debug(debug)

                
-- --------------------------------------------------------------------------
-- Class Button
-- --------------------------------------------------------------------------   

class (Synchronized w, Widget w) => ButtonWidget w where
        flash   :: w -> IO ()
        invoke  :: w -> IO ()
        flash w  = do {try(execMethod w (\ nm -> tkFlash nm)); done}
        invoke w = synchronize w (do {
                t <- isPackedWidget wid;
                if t then
                        execMethod wid (\ nm -> tkInvoke nm)
                else
                        emitGUIEvent wid ButtonClicked NoEventInfo
                }) where wid = toGUIObject w


                
-- --------------------------------------------------------------------------
-- SelectButton
-- --------------------------------------------------------------------------   

class ButtonWidget w => SelectButton w where
        selectionState    :: Toggle -> Config w
        getSelectionState :: w -> IO Toggle
        selectionStateSet :: w -> EV Toggle

        selectionState On w     =  synchronize w (do {
                t <- isPackedWidget wid;
                (if t then
                        execMethod wid (\ nm -> tkSelect nm)
                else
                        emitGUIEvent wid ButtonClicked NoEventInfo);
                return w
                }) where wid    = toGUIObject w

        selectionState Off w    =  synchronize w (do {
                t <- isPackedWidget wid;
                (if t then
                        execMethod wid (\ nm -> tkDeselect nm)
                else
                        emitGUIEvent wid ButtonClicked NoEventInfo);
                return w
                }) where wid    = toGUIObject w


-- --------------------------------------------------------------------------
-- Accelerator
-- --------------------------------------------------------------------------   

class Widget w => HasAccelerator w where
        accelerator    :: String -> Config w
        getAccelerator :: w -> IO String
        accelerator s w = cset w "accelerator" s
        getAccelerator w = cget w "accelerator"



-- --------------------------------------------------------------------------
-- Toggle buttons
-- --------------------------------------------------------------------------   

class SelectButton w => ToggleButton w where 
        toggleButton   :: w -> IO ()
        toggleButton w   =  synchronize w (do {
                t <- isPackedWidget wid;
                if t then
                        execMethod wid (\ nm -> tkToggle nm)
                else
                        emitGUIEvent wid ButtonClicked NoEventInfo
                }) where wid = toGUIObject w



-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Widget GUIOBJECT
instance ButtonWidget GUIOBJECT
instance SelectButton GUIOBJECT
instance ToggleButton GUIOBJECT
instance HasAccelerator GUIOBJECT


-- --------------------------------------------------------------------------
--  Aux. Button Commands
-- --------------------------------------------------------------------------

buttonColours :: HasColour w => w -> ConfigID -> Bool
buttonColours w "background" = True
buttonColours w "foreground" = True
buttonColours w "activebackground" = True
buttonColours w "activeforeground" = True
buttonColours w "disabledforeground" = True
buttonColours w _ = False


-- --------------------------------------------------------------------------
--  Unparsing of Button Commands
-- --------------------------------------------------------------------------

tkFlash :: ObjectName -> TclScript
tkFlash (MenuItemName name i) = []
tkFlash name = [show name ++ " flash"]
{-# INLINE tkFlash #-}

tkInvoke :: ObjectName -> TclScript
tkInvoke (MenuItemName name i) = [show name ++ " invoke " ++ (show i)]
tkInvoke name = [show name ++ " invoke"]
{-# INLINE tkInvoke #-}

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


-- --------------------------------------------------------------------------
-- Button Methods
-- --------------------------------------------------------------------------

buttonMethods :: Methods
buttonMethods = 
        Methods 
                tkGetButtonConfig 
                tkSetButtonConfigs
                tkCreateButtonWidget
                tkPackButtonWidget 
                (destroyCmd defMethods)  
                (cleanupCmd defMethods)
                tkBindButton
                tkUnbindButton


tkGetButtonConfig name "accelerator" = []
tkGetButtonConfig name cid = (cgetCmd defMethods) name cid


tkSetButtonConfigs name args = (csetCmd defMethods) name (validButtonArgs args)

tkCreateButtonWidget kind name oid args = 
        (createCmd defMethods) kind name oid (validButtonArgs args)

tkPackButtonWidget  _ _ name opts oid binds = 
        ("pack " ++ (show name) ++ " " ++ (showConfigs opts)) :
         concatMap (tkBindButton name oid) binds


validButtonArgs args =
        (filter (not . (== "accelerator") . first) args) 
        where first (a,b) = a


tkBindButton ::ObjectName -> ObjectID -> Binding -> TclScript
tkBindButton name oid (tkev,_) | tkev == show ButtonClicked = 
         tkSetButtonConfigs name [("command",toGUIValue (TkCommand (tkButtonCmd oid)))]      
tkBindButton name oid ev = (bindCmd defMethods) name oid ev


tkUnbindButton :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindButton name oid (tkev,_) | (tkev == show ButtonClicked) = 
         tkSetButtonConfigs name [("command",toGUIValue (TkCommand ""))]      
tkUnbindButton name oid ev = (unbindCmd defMethods) name oid ev

                
-- --------------------------------------------------------------------------
--  Item Methods
-- --------------------------------------------------------------------------

activeEntryMethods =
        Methods
                tkGetMenuItemConfig
                tkSetMenuItemConfigs                    
                tkCreateMenuItem                
                tkPackMenuItem          
                (destroyCmd defMethods)
                (cleanupCmd defMethods)
                tkBindMenuItem
                tkUnbindMenuItem


-- --------------------------------------------------------------------------
--  Menu Button Methods
-- --------------------------------------------------------------------------

passiveEntryMethods =
        Methods
                tkGetMenuItemConfig
                tkSetMenuItemConfigs                    
                tkCreateMenuItem                
                tkPackMenuItem          
                (destroyCmd defMethods)
                (cleanupCmd defMethods)
                tkBindMenuItem          
                tkUnbindMenuItem


-- --------------------------------------------------------------------------
--  Unparsing of Menu Commands
-- --------------------------------------------------------------------------

tkCreateMenuItem :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateMenuItem kind menu _ args = tkCreateMenuItem' kind menu args'
        where args' = filter (not . isIllegalMenuItemConfig . first) args


tkCreateMenuItem' :: ObjectKind -> ObjectName -> [ConfigOption] -> TclScript
tkCreateMenuItem' CLICKBUTTON menu opts =
         [(show menu) ++ " add command " ++ (showECO opts)]
tkCreateMenuItem' MENUBUTTON menu opts =
         [(show menu) ++ " add cascade " ++ (showECO opts)]
tkCreateMenuItem' kind menu opts =
         [(show menu) ++ " add " ++ (show kind) ++ " " ++ (showECO opts)]



tkPackMenuItem :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> ObjectID -> [Binding] -> [TclCmd]
tkPackMenuItem  _ _ name _ oid [] = []
tkPackMenuItem  _ _ name _ oid binds = 
         concatMap (tkBindMenuItem name oid) binds


tkRegisterActiveItem :: Maybe ObjectName -> State -> [Binding] -> (Bool,TclScript)
tkRegisterActiveItem (Just name) state [] = 
        (True,tkSetMenuItemConfigs name [("state", toGUIValue state)])
tkRegisterActiveItem Nothing state [] = (True,[])
tkRegisterActiveItem name state _ = (False,[])



tkBindMenuItem ::ObjectName -> ObjectID -> Binding -> TclScript
tkBindMenuItem name oid (tkev,_) | tkev == show ButtonClicked = 
         tkSetMenuItemConfigs' name [("command",toGUIValue (TkCommand (tkButtonCmd oid)))]      
tkBindMenuItem name oid ev = []


tkUnbindMenuItem :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindMenuItem name oid (tkev,_) | (tkev == show ButtonClicked) = 
         tkSetMenuItemConfigs name [("command",toGUIValue (TkCommand ""))]      
tkUnbindMenuItem name oid ev = []


tkGetMenuItemConfig :: ObjectName -> ConfigID -> TclScript
tkGetMenuItemConfig (MenuItemName name i) "text" =      
        [(show name) ++ " entrycget " ++ (show i) ++ " -label"]
tkGetMenuItemConfig (MenuItemName name i) cid | (isIllegalMenuItemConfig cid )= []
tkGetMenuItemConfig (MenuItemName name i) cid = 
        [show name ++ " entrycget " ++ show i ++ " -" ++ cid]
tkGetMenuItemConfig _ _ = []    


tkSetMenuItemConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetMenuItemConfigs name args = 
        tkSetMenuItemConfigs' name args'
        where args' = filter (not . isIllegalMenuItemConfig . first) args


first (a,b) = a


tkSetMenuItemConfigs' :: ObjectName -> [ConfigOption] -> TclScript
tkSetMenuItemConfigs' _ [] = []
tkSetMenuItemConfigs' (MenuItemName name i) args = 
        [show name ++ " entryconfigure " ++ (show i) ++ " " ++ showECO args]
tkSetMenuItemConfigs' _ _ = []


showECO :: [ConfigOption] -> String
showECO [] = ""
showECO (("text",v) : ecl) = (showConfig ("label",v)) ++ " " ++ (showECO ecl)
showECO (x : ecl) =  (showConfig x) ++ " " ++ (showECO ecl)


-- --------------------------------------------------------------------------
--  Filtering of Configuration Configs
-- --------------------------------------------------------------------------

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
isIllegalMenuItemConfig "padx" = True
isIllegalMenuItemConfig "pady" = True
isIllegalMenuItemConfig _ = False



{- #########################################################################

MODULE        : Toplevel
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The toplevel widget. This widget maintains the configuration
                options and window manager attributes as well.


   ######################################################################### -}

module Toplevel (
        Toplevel(..),

        newToplevel     
        ) where

import Concurrency
import Resources
import GUIObject
import GUIState
import GUIRealise
import GUIBaseClasses
import GUIInteraction
import Packer
import List
import Debug(debug)


-- --------------------------------------------------------------------------
-- Toplevel Widget
-- --------------------------------------------------------------------------           
newtype Toplevel = Toplevel GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Creation Commands
-- --------------------------------------------------------------------------           
newToplevel :: Widget w => w -> [Config Toplevel] -> IO Toplevel
newToplevel w confs = do {
        kind <- getObjectKind wid;
        tp <- (case kind of
                MENU -> do {
                        deleteConfig wid "screen";  -- tk menus are local
                        setMethods wid toplevelMethods;
                        return (Toplevel wid)
                        }
                TOPLEVEL -> configure (Toplevel (toGUIObject w)) confs
                _ -> do {
                        tl <- createToplevel;
                        widtl <- return (toGUIObject tl);
                        pset tl "fill" "both";
                        pset tl "expand" On;
                        makeChildObject widtl wid;
                        return (Toplevel widtl)                 
                        });
        configure tp confs
} where wid = (toGUIObject w)
        createToplevel = 
                createGUIObject TOPLEVEL toplevelMethods >>=  
                return . Toplevel


-- --------------------------------------------------------------------------
--  Instantiations
-- --------------------------------------------------------------------------           
instance GUIObject Toplevel where 
        toGUIObject (Toplevel f) = f
        cname _ = "Toplevel"

instance Destructible Toplevel where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive Toplevel

instance Widget Toplevel

instance Synchronized Toplevel where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
-- Toplevel Methods
-- --------------------------------------------------------------------------

toplevelMethods =
        Methods
                tkGetToplevelConfig
                tkSetToplevelConfigs
                tkCreateToplevel
                tkPackToplevel
                (destroyCmd defMethods)
                (cleanupCmd defMethods)
                tkBindToplevel
                tkUnbindToplevel

-- --------------------------------------------------------------------------
--  Auxiliary Functions
-- --------------------------------------------------------------------------

isWMConfig :: ConfigID -> Bool
isWMConfig "state" = True 
isWMConfig "geometry" = True 
isWMConfig "minsize" = True 
isWMConfig "maxsize" = True 
isWMConfig "aspect" = True 
isWMConfig "sizefrom" = True 
isWMConfig "positionfrom" = True 
isWMConfig "title" = True 
isWMConfig "transient" = True 
isWMConfig "group" = True 
isWMConfig "iconname" = True 
isWMConfig "iconbitmap" = True 
isWMConfig "iconposition" = True 
isWMConfig "iconmask" = True 
isWMConfig "focusmodel" = True 
isWMConfig _ = False 


-- --------------------------------------------------------------------------
-- Unparsing of Commands
-- --------------------------------------------------------------------------

tkCreateToplevel :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateToplevel kind name oid args =
        [ show kind ++ " " ++ show name ++ " " ++ showConfigs cargs, 
          wmSetConfigs name wargs
        ]
        where (wargs,cargs) = partition (\(cid,_) -> isWMConfig cid) args
{-# INLINE tkCreateToplevel #-}


tkGetToplevelConfig :: ObjectName -> ConfigID -> TclScript
tkGetToplevelConfig name cid | isWMConfig cid = 
        ["wm " ++ cid ++ " " ++ (show name)]
tkGetToplevelConfig name cid = 
        [(show name) ++ " cget -" ++ cid]
{-# INLINE tkGetToplevelConfig #-}


tkSetToplevelConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetToplevelConfigs _ [] = []
tkSetToplevelConfigs name args = 
        [cSetConfigs name cargs, wmSetConfigs name wargs]
        where (wargs,cargs) = partition (\(cid,_) -> isWMConfig cid) args
{-# INLINE tkSetToplevelConfigs #-}


tkPackToplevel :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> 
                ObjectID -> [Binding] -> TclScript
tkPackToplevel t _ name _ oid binds = 
                (tkCreateWindow name) ++ (tkDeclBindings name oid binds)
{-# INLINE tkPackToplevel #-}


tkBindToplevel ::ObjectName -> ObjectID -> Binding -> TclScript
tkBindToplevel _ _ (tkev,_) | tkev == show WindowDestroyed = []
tkBindToplevel _ _ (tkev,_) | tkev == show SaveYourself = []
tkBindToplevel name oid ev = (bindCmd defMethods) name oid ev
{-# INLINE tkBindToplevel #-}


tkUnbindToplevel :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindToplevel _ _ (tkev,_) | (tkev == show WindowDestroyed) = []
tkUnbindToplevel _ _ (tkev,_) | (tkev == show SaveYourself) = []
tkUnbindToplevel name oid ev = (unbindCmd defMethods) name oid ev
{-# INLINE tkUnbindToplevel #-}


tkCreateWindow :: ObjectName -> TclScript
tkCreateWindow name = [
        "wm protocol " ++ show name ++ " WM_DELETE_WINDOW " ++ 
         "{puts stdout {EV " ++ (drop 2 (show name)) ++ " WD }; flush stdout}",
        "wm protocol " ++ show name ++ " WM_SAVE_YOURSELF " ++ 
          "{puts stdout {EV " ++ (drop 2 (show name)) ++ " SY }; flush stdout}"
        ]


cSetConfigs :: ObjectName -> [ConfigOption] -> TclCmd
cSetConfigs name [] = ""
cSetConfigs name args = show name ++ " configure " ++ showConfigs args


wmSetConfigs :: ObjectName -> [ConfigOption] -> TclCmd
wmSetConfigs name [] = ""
wmSetConfigs name ((cid,val) : args) = 
        wmSet name cid val ++ ";" ++ wmSetConfigs name args


wmSet :: ObjectName -> ConfigID -> GUIVALUE -> TclCmd
wmSet name "state" val = "wm " ++ show val ++ " " ++ show name
wmSet name cid val = "wm " ++ cid ++ " " ++ show name ++  " " ++ show val


{-# LANGUAGE OverlappingInstances #-}

-- | HTk\'s /toplevel/ widget.
-- A toplevel widget is a toplevel container for widgets (a window).
module HTk.Containers.Toplevel (

  Toplevel(..),

  createToplevel,

  tkGetToplevelConfig,
  tkSetToplevelConfigs

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses
import Data.List
import Util.Computation
import Events.Destructible
import Events.Synchronized
import HTk.Containers.Window
import HTk.Kernel.Packer


-- -----------------------------------------------------------------------
-- Toplevel widget
-- -----------------------------------------------------------------------

-- | The @Toplevel@ datatype.
newtype Toplevel = Toplevel GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation commands
-- -----------------------------------------------------------------------

-- | Constructs a new toplevel widget and returns a handler.
createToplevel :: [Config Toplevel]
   -- ^ the list of configuration options for this toplevel
   -- widget.
   -> IO Toplevel
   -- ^ A toplevel widget.
createToplevel cnf =
  do
    wid <- createGUIObject ROOT TOPLEVEL toplevelMethods
    configure (Toplevel wid) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Toplevel where
  toGUIObject (Toplevel f) = f
  cname _ = "Toplevel"

-- | A toplevel widget can be destroyed.
instance Destroyable Toplevel where
  --  Destroys a toplevel widget.
  destroy = destroy . toGUIObject

-- | A toplevel widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Toplevel

-- | A toplevel widget is a container for widgets. You can pack widgets to
-- a toplevel widget via pack or grid command in the
-- @module HTk.Kernel.Packer@.
instance Container Toplevel

-- | You can synchronize on a toplevel object.
instance Synchronized Toplevel where
  --  Synchronizes on a toplevel object.
  synchronize = synchronize . toGUIObject

-- | A toplevel widget is a window (with various configurations and actions
-- concerning its stacking order, display status, screen, aspect ratio
-- etc.).
instance Window Toplevel


-- -----------------------------------------------------------------------
-- toplevel methods
-- -----------------------------------------------------------------------

toplevelMethods = Methods tkGetToplevelConfig
                          tkSetToplevelConfigs
                          tkCreateToplevel
                          (packCmd voidMethods)
                          (gridCmd voidMethods)
                          (destroyCmd defMethods)
                          (bindCmd defMethods)
                          (unbindCmd defMethods)
                          (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- Unparsing of Commands
-- -----------------------------------------------------------------------

tkCreateToplevel :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                    [ConfigOption] -> TclScript
tkCreateToplevel _ kind name _ args =
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

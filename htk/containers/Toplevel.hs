-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Toplevel (

  Toplevel(..),

  createToplevel,

  tkGetToplevelConfig,
  tkSetToplevelConfigs

) where

import Resources
import Core
import BaseClasses
import List
import Computation
import Destructible
import Synchronized
import Window
import Packer


-- -----------------------------------------------------------------------
-- Toplevel widget
-- -----------------------------------------------------------------------

newtype Toplevel = Toplevel GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation commands
-- -----------------------------------------------------------------------

createToplevel :: [Config Toplevel] -> IO Toplevel
createToplevel confs =
  do
    wid <- createGUIObject ROOT TOPLEVEL toplevelMethods
    configure (Toplevel wid) confs


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance GUIObject Toplevel where 
  toGUIObject (Toplevel f) = f
  cname _ = "Toplevel"

instance Destroyable Toplevel where
  destroy = destroy . toGUIObject

instance Widget Toplevel

instance Container Toplevel

instance Synchronized Toplevel where
  synchronize = synchronize . toGUIObject

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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Description: Low-Level Tcl\/Tk interface
module HTk.Toplevel.HTk (

  requirePackage,
  forgetPackage,
  isPackageAvailable,
  isTixAvailable,


-- basic ressources

  module HTk.Kernel.Resources,
  module HTk.Kernel.GUIValue,
  module HTk.Kernel.Font,
  module HTk.Kernel.Geometry,
  module HTk.Kernel.Colour,
  module HTk.Kernel.Tooltip,
  module HTk.Kernel.TkVariables,
  module Events.Synchronized,
  module Util.Computation,
  module HTk.Kernel.Configuration,
  module HTk.Kernel.BaseClasses,
  module HTk.Kernel.Cursor,


-- text items

  module HTk.Textitems.TextTag,
  module HTk.Textitems.Mark,
  module HTk.Textitems.EmbeddedTextWin,


-- window submodules

  module HTk.Containers.Window,
  module HTk.Containers.Toplevel,


-- widget submodules

  module HTk.Containers.Frame,
  module HTk.Widgets.Label,
  module HTk.Widgets.Message,
  module HTk.Widgets.Entry,
  module HTk.Widgets.Button,
  module HTk.Widgets.CheckButton,
  module HTk.Widgets.RadioButton,
  module HTk.Widgets.MenuButton,
  module HTk.Widgets.Canvas,
  module HTk.Widgets.Editor,
  module HTk.Widgets.ListBox,
  module HTk.Widgets.OptionMenu,
  module HTk.Widgets.Scale,
  module HTk.Widgets.ScrollBar,
  module HTk.Devices.Screen,
  module HTk.Containers.Box,


-- tix submodules

  module HTk.Tix.NoteBook,
  module HTk.Tix.LabelFrame,
  module HTk.Tix.PanedWindow,
  module HTk.Widgets.ComboBox,


-- devices submodules

  module HTk.Devices.Bell,
  module HTk.Devices.Printer,


-- menu / menuitem submodules

  module HTk.Menuitems.Menu,
  module HTk.Menuitems.MenuCascade,
  module HTk.Menuitems.MenuCommand,
  module HTk.Menuitems.MenuCheckButton,
  module HTk.Menuitems.MenuRadioButton,
  module HTk.Menuitems.MenuSeparator,


-- canvasitem submodules

  module HTk.Canvasitems.CanvasItem,
  module HTk.Canvasitems.Arc,
  module HTk.Canvasitems.Line,
  module HTk.Canvasitems.Oval,
  module HTk.Canvasitems.Polygon,
  module HTk.Canvasitems.Rectangle,
  module HTk.Canvasitems.ImageItem,
  module HTk.Canvasitems.BitMapItem,
  module HTk.Canvasitems.TextItem,
  module HTk.Canvasitems.CanvasTag,
  module HTk.Canvasitems.EmbeddedCanvasWin,


-- components submodules

  module HTk.Components.Index,
  module HTk.Components.BitMap,
  module HTk.Components.Image,
  module HTk.Components.Focus,
  module HTk.Components.Icon,
  module HTk.Components.Selection,

-- widget packing

  module HTk.Kernel.ButtonWidget,
  module HTk.Kernel.Packer,
  module HTk.Kernel.PackOptions,
  module HTk.Kernel.GridPackOptions,


-- events

  module Events.Events,
  module HTk.Kernel.EventInfo,
  module Events.Spawn,
  module Events.Channels,
  WishEvent(..),
  WishEventType(..),
  WishEventModifier(..),
  KeySym(..),
  bind,
  bindSimple,
  bindPath,
  bindPathSimple,
  HasCommand(..),

  delayWish,

-- other basic stuff     // TD: sort out!

  initHTk, -- :: [Config HTk] -> IO HTk
  -- initHTk initialises HTk.

  withdrawMainWin, -- :: Config HTk
  -- withDraw as a configuration

  resourceFile, -- :: String-> Config HTk
  -- loads resource file

  finishHTk, -- :: IO ()
  -- waits for all wish to finish and then terminates

  withdrawWish, -- :: IO ()
  -- withdrawWish withdraws the wish window.

  HTk,

  updateAllTasks,
  updateIdleTasks,

  Destructible(..),
  Destroyable(..),

  cleanupWish,

  getHTk,
) where

import Control.Concurrent

import Events.Channels
import Events.Destructible
import Events.Events
import Events.Spawn
import Events.Synchronized

import HTk.Canvasitems.Arc
import HTk.Canvasitems.BitMapItem
import HTk.Canvasitems.CanvasItem hiding (Canvas)
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.EmbeddedCanvasWin
import HTk.Canvasitems.ImageItem
import HTk.Canvasitems.Line
import HTk.Canvasitems.Oval
import HTk.Canvasitems.Polygon
import HTk.Canvasitems.Rectangle
import HTk.Canvasitems.TextItem

import HTk.Components.BitMap
import HTk.Components.Focus
import HTk.Components.Icon
import HTk.Components.Image
import HTk.Components.Index
import HTk.Components.Selection

import HTk.Containers.Box
import HTk.Containers.Frame
import HTk.Containers.Toplevel
import HTk.Containers.Window

import HTk.Devices.Bell
import HTk.Devices.Printer
import HTk.Devices.Screen

import HTk.Kernel.BaseClasses
import HTk.Kernel.ButtonWidget
import HTk.Kernel.Colour
import HTk.Kernel.Configuration
import HTk.Kernel.Core
import HTk.Kernel.Cursor
import HTk.Kernel.EventInfo
import HTk.Kernel.Font
import HTk.Kernel.GUIValue
import HTk.Kernel.Geometry
import HTk.Kernel.GridPackOptions
import HTk.Kernel.PackOptions
import HTk.Kernel.Packer
import HTk.Kernel.Resources
import HTk.Kernel.TkVariables
import HTk.Kernel.Tooltip
import HTk.Kernel.Wish

import HTk.Menuitems.Menu
import HTk.Menuitems.MenuCascade
import HTk.Menuitems.MenuCheckButton
import HTk.Menuitems.MenuCommand
import HTk.Menuitems.MenuRadioButton
import HTk.Menuitems.MenuSeparator

import HTk.Textitems.EmbeddedTextWin
import HTk.Textitems.Mark
import HTk.Textitems.TextTag

import HTk.Tix.LabelFrame
import HTk.Tix.NoteBook
import HTk.Tix.PanedWindow

import HTk.Widgets.Button
import HTk.Widgets.Canvas
import HTk.Widgets.CheckButton
import HTk.Widgets.ComboBox
import HTk.Widgets.Editor
import HTk.Widgets.Entry
import HTk.Widgets.Label
import HTk.Widgets.ListBox
import HTk.Widgets.MenuButton
import HTk.Widgets.Message
import HTk.Widgets.OptionMenu
import HTk.Widgets.RadioButton
import HTk.Widgets.Scale
import HTk.Widgets.ScrollBar

import System.IO.Unsafe
import Util.Computation

-- -----------------------------------------------------------------------
-- type HTk and its instances
-- -----------------------------------------------------------------------

-- | The @HTk@ datatype - a handle for the wish instance and
-- the main window.
newtype HTk = HTk GUIOBJECT

-- | Internal.
instance GUIObject HTk where
  toGUIObject (HTk obj) = obj
  cname _ = "HTk"

-- | Internal.
instance Eq HTk where
  (HTk obj1) == (HTk obj2) = obj1 == obj2

-- | The wish instance can be destroyed.
instance Destroyable HTk where
  -- Destroys the wish instance.
  destroy = destroy . toGUIObject

-- | The wish instance is associated with the main window (with various
-- configurations and actions concerning its stacking order, display
-- status, screen, aspect ratio etc.).
instance Window HTk

-- | The main window is a container for widgets. You can pack widgets to
-- the main window via pack or grid command in the
-- @module HTk.Kernel.Packer@.
instance Container HTk

-- | You can synchronize on the wish instance.
instance Synchronized HTk where
  -- Synchronizes on the wish instance.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

--- @doc initHTk
-- Only one HTk is allowed to exist, of course.  It is initialised
-- by whichever of getHTk and initHTk is called first; once initialised
-- initHTk may not be called again.  So in general, where initHTk is
-- used, you should use it before any other HTk action.
theHTkMVar :: MVar (Maybe HTk)
theHTkMVar = unsafePerformIO (newMVar Nothing)
{-# NOINLINE theHTkMVar #-}

-- | Initializes HTk.
initHTk :: [Config HTk]
   -- ^ the list of configuration options for the wish
   -- instance \/ main window.
   -> IO HTk
   -- ^ The wish instance.
initHTk cnf =
  do
    htkOpt <- takeMVar theHTkMVar
    htk <- case htkOpt of
       Nothing -> newHTk cnf
       Just htk -> return htk -- should we configure again?
    putMVar theHTkMVar (Just htk)
    return htk



--- @doc getHTk
-- getHTk retrieves the current HTk (initialising if necessary).
getHTk :: IO HTk
getHTk =
   do
      htkOpt <- takeMVar theHTkMVar
      htk <- case htkOpt of
         Nothing -> newHTk []
         Just htk -> return htk
      putMVar theHTkMVar (Just htk)
      return htk

--- @doc newHTk
-- newHTk actually creates a new HTk.  DO NOT call this except
-- by initHTk or getHTk!
newHTk :: [Config HTk] -> IO HTk
newHTk opts =
   do
      obj <- createHTkObject htkMethods
      configure (HTk obj) opts
      return (HTk obj)

--- @doc withdrawWish
-- withdrawWish withdraws the wish window.
withdrawWish :: IO ()
withdrawWish =
   do
      htk <- getHTk
      withdraw htk

-- | Withdraws the main window.
withdrawMainWin :: Config HTk
withdrawMainWin htk =
  do
    withdraw htk
    return htk

--- @doc readResourceFile
-- Load a resource file
-- A resource files specifies the default options for fonts, colours, &c.
resourceFile :: String-> Config HTk
resourceFile file htk =
  do execCmd ("option readfile "++ file++ " startup")
                    -- "startup" is the priority; we could make this user-
                    -- configurable if wished?
     return htk

--- @doc finishHTk
-- waits for HTk to finish, and calls cleanupWish to clean up.
-- This rebinds the Destroy event of the main window, so
-- do not call this function if you have bound anything to that.
-- In that case, call cleanupWish after you have finished with wish.
finishHTk :: IO ()
finishHTk =
   do htk <- getHTk
      (htk_destr, _) <- bindSimple htk Destroy
      sync htk_destr
      cleanupWish


-- -----------------------------------------------------------------------
-- HTk methods
-- -----------------------------------------------------------------------

htkMethods = Methods tkGetToplevelConfig
                     tkSetToplevelConfigs
                     (createCmd voidMethods)
                     (packCmd voidMethods)
                     (gridCmd voidMethods)
                     (destroyCmd defMethods)
                     (bindCmd defMethods)
                     (unbindCmd defMethods)
                     (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- application updates
-- -----------------------------------------------------------------------

-- | Updates all tasks.
updateAllTasks :: IO ()
updateAllTasks = execTclScript ["update"]

-- | Updates idle tasks.
updateIdleTasks :: IO ()
updateIdleTasks = execTclScript ["update idletasks"]


-- -----------------------------------------------------------------------
-- application Name
-- -----------------------------------------------------------------------

-- | The wish instance has a value - the application name.
instance GUIValue v => HasValue HTk v where
  -- Sets the application name.
  value aname htk =
    do
      execTclScript ["tk appname " ++ show aname]
      return htk
  -- Gets the application name.
  getValue _ = evalTclScript ["tk appname"] >>= creadTk

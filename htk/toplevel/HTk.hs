
module HTk (

  requirePackage,
  forgetPackage,
  isPackageAvailable,
  isTixAvailable,


-- basic ressources

  module Resources,
  module GUIValue,
  module Font,
  module Geometry,
  module Colour,
  module Tooltip,
  module TkVariables,
  module Synchronized,
  module Computation,
  module Configuration,
  module BaseClasses,
  module Cursor,


-- text items

  module TextTag,
  module Mark,
  module EmbeddedTextWin,


-- window submodules

  module Window,
  module Toplevel,


-- widget submodules

  module Frame,
  module Label,
  module Message,
  module Entry,
  module Button,
  module CheckButton,
  module RadioButton,
  module MenuButton,
  module Canvas,
  module Editor,
  module ListBox,
  module OptionMenu,
  module Scale,
  module ScrollBar,
  module Screen,
  module Box,


-- tix submodules

  module NoteBook,
  module LabelFrame,
  module PanedWindow,
  module ComboBox,


-- devices submodules

  module Bell,
  module Printer,


-- menu / menuitem submodules

  module Menu,
  module MenuCascade,
  module MenuCommand,
  module MenuCheckButton,
  module MenuRadioButton,
  module MenuSeparator,


-- canvasitem submodules

  module CanvasItem,
  module Arc,
  module Line,
  module Oval,
  module Polygon,
  module Rectangle,
  module ImageItem,
  module BitMapItem,
  module TextItem,
  module CanvasTag,
  module EmbeddedCanvasWin,


-- components submodules

  module BitMap,
  module Image,
  module Focus,
  module Icon,

-- widget packing

  module Packer,
  module PackOptions,
  module GridPackOptions,


-- events

  module Events,
  module EventInfo,
  module Spawn,
  module Channels,
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
  AbstractWidget(..),    -- TD: needed ?

  updateAllTasks,
  updateIdleTasks, 

  Destructible(..),
  Destroyable(..),

  done,
  cleanupWish,

  getHTk,
) where

import Concurrent
import qualified IOExts(unsafePerformIO)

import Frame
import Label
import Message
import Entry
import Button
import CheckButton
import RadioButton
import MenuButton
import Canvas
import Editor
import ListBox
import OptionMenu
import Scale
import ScrollBar
import Menu
import MenuCascade
import MenuCommand
import MenuCheckButton
import MenuRadioButton
import MenuSeparator
import CanvasItem
import Arc
import Line
import Oval
import Polygon
import Rectangle
import ImageItem
import BitMapItem
import TextItem
import CanvasTag
import EmbeddedCanvasWin
import Image
import BitMap
import Configuration
import Packer
import PackOptions
import GridPackOptions
import Screen
import Cursor
import Computation
import Geometry
import Resources
import Tooltip
import Font
import Colour
import GUIValue
import Core
import BaseClasses
import Box
import Toplevel
import Window
import ReferenceVariables
import Destructible
import EventInfo
import Events
import Spawn
import Channels
import Synchronized
import TkVariables
import TextTag
import Mark
import Wish
import EmbeddedTextWin
import NoteBook
import LabelFrame
import PanedWindow
import ComboBox
import Bell
import Focus
import Icon
import Printer


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
-- @module Packer@.
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
theHTkMVar = IOExts.unsafePerformIO (newMVar Nothing)
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

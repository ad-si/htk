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

module HTk (

  tixAvailable,

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

  module Box,    -- TD: does not work properly yet!!


-- tix submodules

  module NoteBook,
  module LabelFrame,
  module PanedWindow,


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


-- image submodules

  module BitMap,
  module Image,


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
  BNo(..),
  KeySym(..),
  bind,
  bindSimple,
  HasCommand(..),

  delayWish,

-- other basic stuff     // TD: sort out!

  initHTk, -- :: [Config HTk] -> IO HTk
  -- initHTk initialises HTk.

  finishHTk, -- :: Htk-> IO ()
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
  cleanupWish
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
--import Icon
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


data AbstractWidget = NONE
instance GUIObject AbstractWidget where
  toGUIObject _ = ROOT
instance Container AbstractWidget


-- -----------------------------------------------------------------------
-- type HTk and its instances
-- -----------------------------------------------------------------------

newtype HTk = HTk GUIOBJECT

instance GUIObject HTk where
  toGUIObject (HTk obj) = obj
  cname _ = "HTk"

instance Eq HTk where 
  (HTk obj1) == (HTk obj2) = obj1 == obj2

instance Destroyable HTk where
  destroy = destroy . toGUIObject

instance Window HTk

instance Container HTk


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

initHTk :: [Config HTk] -> IO HTk
initHTk opts =                            -- config (TD)
  do
    htkOpt <- takeMVar theHTkMVar
    htk <- case htkOpt of
       Nothing -> newHTk opts
       Just htk -> error "HTk.initHTk called when HTk is already initialised!"
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


--- @doc finishHTk
-- waits for HTk to finish, and calls cleanupWish to clean up. 
-- This rebinds the Destroy event of the main window, so 
-- do not call this function if you have bound anything to that.
-- In that case, call cleanupWish after you have finished with wish.
finishHTk :: HTk-> IO ()
finishHTk main =
   do (htk_destr, _) <- bindSimple main Destroy
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

updateAllTasks :: IO ()
updateAllTasks = execTclScript ["update"]

updateIdleTasks :: IO ()
updateIdleTasks = execTclScript ["update idletasks"]


-- -----------------------------------------------------------------------
-- application Name
-- -----------------------------------------------------------------------

instance GUIValue v => HasValue HTk v where
  value aname htk =
    do
      execTclScript ["tk appname " ++ show aname]
      return htk
  getValue _ = evalTclScript ["tk appname"] >>= creadTk

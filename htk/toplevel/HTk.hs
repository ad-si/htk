{- ####################################################################

MODULE        : HTk
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Haskell Tk.

   #################################################################### -}


module HTk (

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


-- other basic stuff     // TD: sort out!

  initHTk,
  HTk,
  AbstractWidget(..),    -- TD: needed ?

  updateAllTasks,
  updateIdleTasks, 

  Destructible(..),
  Destroyable(..),

  done,
) where

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
import TextTag
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

initHTk :: [Config HTk] -> IO HTk
initHTk opts =                            -- config (TD)
  do
    obj <- createHTkObject htkMethods
    configure (HTk obj) opts
    return (HTk obj)

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
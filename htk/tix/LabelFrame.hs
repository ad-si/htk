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

---
-- HTk's <strong>LabelFrame</strong> widget.
-- A labelled container for widgets. This widget is from the Tix library
-- and therefore only available if Tix is installed. When Tix is not
-- available, a normal frame widget will be used instead.
module LabelFrame (

  LabelFrame,
  newLabelFrame,

  labelSide,
  getLabelSide,
  LabelSide(..)

) where

import Core
import BaseClasses(Widget)
import Configuration
import Computation
import Synchronized
import Destructible
import Packer
import Char
import PackOptions
import GridPackOptions
import Tooltip

-- -----------------------------------------------------------------------
-- type LabelFrame
-- -----------------------------------------------------------------------

---
-- The <code>LabelFrame</code> datatype.
newtype LabelFrame = LabelFrame GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- labelled frame creation
-- -----------------------------------------------------------------------

---
-- Constructs a new label frame and returns it as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this labelled
--                - frame.
-- @return result - A labelled frame.
newLabelFrame :: Container par => par -> [Config LabelFrame] ->
                                  IO LabelFrame
newLabelFrame par cnf =
  do
    w <- createGUIObject (toGUIObject par) LABELFRAME labelFrameMethods
    configure (LabelFrame w) cnf


-- -----------------------------------------------------------------------
-- widget specific configuration options
-- -----------------------------------------------------------------------

labelSide :: LabelSide -> LabelFrame -> IO LabelFrame
labelSide ls w = cset w "labelside" ls

getLabelSide :: LabelFrame -> IO LabelSide
getLabelSide w = cget w "labelside"

data LabelSide =
    TopLabel | LeftLabel | RightLabel | BottomLabel | NoLabel
  | AcrossTopLabel

instance Read LabelSide where
  readsPrec p b =
    case dropWhile isSpace b of
      't':'o':'p': xs -> [(TopLabel,xs)]
      'l':'e':'f':'t': xs -> [(LeftLabel, xs)]
      'r':'i':'g':'h':'t': xs -> [(RightLabel, xs)]
      'b':'o':'t':'t':'o':'m': xs -> [(BottomLabel, xs)]
      'n':'o':'n':'e': xs -> [(NoLabel, xs)]
      'a':'c':'r':'o':'s':'s':'t':'o':'p': xs -> [(AcrossTopLabel, xs)]
      _ -> []

instance Show LabelSide where
  showsPrec d p r =
    (case p of TopLabel -> "top"
               LeftLabel -> "left"
               RightLabel -> "right"
               BottomLabel -> "bottom"
               NoLabel -> "none"
               AcrossTopLabel -> "acrosstop") ++ r

instance GUIValue LabelSide where
  cdefault = TopLabel


-- -----------------------------------------------------------------------
-- labelled frame methods
-- -----------------------------------------------------------------------

labelFrameMethods = Methods tkGetLabelFrameConfig
                            tkSetLabelFrameConfigs
                            tkCreateLabelFrame
                            tkPackLabelFrame
                            tkGridLabelFrame
                            (destroyCmd defMethods)
                            (bindCmd defMethods)
                            (unbindCmd defMethods)
                            (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- unparsing of labelled frame commands
-- -----------------------------------------------------------------------

tkGetLabelFrameConfig :: ObjectName -> ConfigID -> TclScript
tkGetLabelFrameConfig (LabelFrameName nm oid) cid =
  [show nm ++ " cget -" ++ cid]
{-# INLINE tkGetLabelFrameConfig #-}

tkSetLabelFrameConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetLabelFrameConfigs (LabelFrameName nm oid) args =
  [show nm ++ " configure " ++ showConfigs args]
tkSetLabelFrameConfigs _ _ = []
{-# INLINE tkSetLabelFrameConfigs #-}

tkCreateLabelFrame :: ObjectName -> ObjectKind -> ObjectName ->
                      ObjectID -> [ConfigOption] -> TclScript
tkCreateLabelFrame parnm _ nm oid args =
  ["tixLabelFrame " ++ show parnm ++ "." ++ show oid ++ " "++
   showConfigs args,
   "global v" ++ show oid,
   "set v" ++ show oid ++ " [" ++ show parnm ++ "." ++ show oid ++ " subwidget frame]"]
{-# INLINE tkCreateLabelFrame #-}

tkPackLabelFrame :: ObjectName -> [PackOption] -> TclScript
tkPackLabelFrame (LabelFrameName nm _) opts =
  ["pack " ++ show nm ++ " " ++ showPackOptions opts]

tkGridLabelFrame :: ObjectName -> [GridPackOption] -> TclScript
tkGridLabelFrame (LabelFrameName nm _) opts =
  ["grid " ++ show nm ++ " " ++ showGridPackOptions opts]


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject LabelFrame where
---
-- Internal.
  toGUIObject (LabelFrame w) = w
---
-- Internal.
  cname _ = "LabelFrame"

---
-- A labelled frame can be destroyed.
instance Destroyable LabelFrame where
---
-- Destroys a labelled frame widget.
  destroy   = destroy . toGUIObject

---
-- A labelled frame has standard widget properties
-- (concerning focus, cursor).
instance Widget LabelFrame

---
-- A labelled frame is a container for widgets. You can pack widgets to
-- a labelled frame via pack or grid command in the
-- <code>module Packer</code>.
instance Container LabelFrame

---
-- A labelled frame has a configureable border.
instance HasBorder LabelFrame

---
-- A labelled frame has a background colour.
instance HasColour LabelFrame where 
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- A labelled frame can have a tooltip.
instance HasTooltip LabelFrame

---
-- You can specify the size of a labelled frame.
instance HasSize LabelFrame

---
-- Sets and gets the string to display as a label for the frame.
instance GUIValue v => HasText LabelFrame v where
---
-- Sets the text to display with the frame.
  text s w  = cset w  "label" s
---
-- Returns the displayed text.
  getText w = cget w "label"

---
-- You can synchronize on a labelled frame (in JAVA style).
instance Synchronized LabelFrame where
---
-- Synchronizes on a label object.
  synchronize = synchronize . toGUIObject

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
-- HTk's <strong>frame widget</strong>.<br>
-- A frame is a simple container for widgets.
module Frame (

  Frame, 
  newFrame

) where

import Core
import BaseClasses(Widget)
import Configuration
import Computation
import Synchronized
import Destructible
import Packer


-- -----------------------------------------------------------------------
-- type Frame
-- -----------------------------------------------------------------------

---
-- The <code>Frame</code> datatype.
data Frame = Frame GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new frame widget and returns a handler as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this frame.
-- @return result - A frame widget.
newFrame :: Container par => par -> [Config Frame] -> IO Frame
newFrame par confs =
  do
    w <- createWidget (toGUIObject par) FRAME
    configure (Frame w) confs


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Frame where
---
-- Internal.
  toGUIObject (Frame w) = w 
---
-- Internal.
  cname _ = "Frame"

---
-- A frame widget can be destroyed.
instance Destroyable Frame where
---
-- Destroys a frame widget.
  destroy   = destroy . toGUIObject

---
-- A frame widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Frame

---
-- A frame widget is a container for widgets. You can pack widgets to
-- a frame widget via pack or grid command in the
-- <code>module Packer</code>.
instance Container Frame

---
-- A frame widget has a configureable border.
instance HasBorder Frame

---
-- A frame widget has a background colour.
instance HasColour Frame where 
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- You can specify the size of a frame.
instance HasSize Frame

---
-- You can synchronize on a label object (in JAVA style).
instance Synchronized Frame where
---
-- Synchronizes on a label object.
  synchronize = synchronize . toGUIObject
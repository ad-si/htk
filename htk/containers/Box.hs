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
-- A container widget with a preset packing orientation (for simple
-- packing).
module Box (

  Flexibility(..),
  Box,

  newBox,
  newHBox,
  newVBox,
  newHFBox,
  newVFBox

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Frame
import Destructible
import Computation
import Synchronized
import ReferenceVariables
import Packer


-- -----------------------------------------------------------------------
-- horizontal/vertical box 
-- -----------------------------------------------------------------------

---
-- The <code>Box</code> datatype.
data Box = Box GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

---
-- Constructs a new box and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param fl      - the flexibility of the box.
-- @param cnf     - the list of configuration options for this box.
-- @return result - A box.
newBox :: Container par => par -> Flexibility -> [Config Box] -> IO Box
newBox par fl cnf =
  do
    w <- createWidget (toGUIObject par) (BOX cdefault fl)
    configure (Box  w) cnf

---
-- Constructs a new box with horizontal packing order and rigid
-- flexibility and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this box.
-- @return result - A box.
newHBox :: Container par => par -> [Config Box] -> IO Box
newHBox par cnf = newBox par Rigid ((orient Horizontal) : cnf)

---
-- Constructs a new box with vertical packing order and rigid
-- flexibility and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this box.
-- @return result - A box.
newVBox :: Container par => par -> [Config Box] -> IO Box
newVBox par cnf = newBox par Rigid ((orient Vertical) : cnf)

---
-- Constructs a new flexible box with horizontal packing order and returns
-- a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this box.
-- @return result - A box.
newHFBox :: Container par => par -> [Config Box] -> IO Box
newHFBox par cnf = newBox par Flexible ((orient Horizontal) : cnf)

---
-- Constructs a new flexible box with vertical packing order and returns
-- a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this box.
-- @return result - A box.
newVFBox :: Container par => par -> [Config Box] -> IO Box
newVFBox par cnf = newBox par Flexible ((orient Vertical) : cnf)


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance Eq Box where 
---
-- Internal.
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

---
-- Internal.
instance GUIObject Box where 
---
-- Internal.
  toGUIObject (Box w) = toGUIObject w
---
-- Internal.
  cname _ = "Box"

---
-- A box can be destroyed.
instance Destroyable Box where
---
-- Destroys a box.
  destroy = destroy . toGUIObject

---
-- You can synchronize on a box object.
instance Synchronized Box where
---
-- Synchronizes on a box object.
  synchronize = synchronize . toGUIObject

---
-- A box has standard widget properties
-- (concerning focus, cursor).
instance Widget Box

---
-- A box is a container for widgets. You can pack widgets to
-- a box via pack or grid command in the <code>module Packer</code>.
instance Container Box

---
-- A box has a configureable border.
instance HasBorder Box

---
-- A box has a configureable background colour.
instance HasColour Box where 
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- A box'es packing orientation is configureable.
instance HasOrientation Box where
---
-- Sets the box'es packing orientation.
  orient or box@(Box w) =
    do
      BOX or' fl <- getObjectKind w
      setObjectKind w (BOX or fl)
      return box
---
-- Gets the box'es packing orientation.
  getOrient (Box w) =
    do
      BOX or _ <- getObjectKind w
      return or

---
-- You can specify the size of a box.
instance HasSize Box

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
-- HTk's <strong>space</strong> widget.<br>
-- A simple spacer for special packing purposes.
module Space (

  Space,
  newSpace,

) where

import Core
import Frame
import Configuration
import Resources
import GUIObject
import Destructible
import Synchronized
import Geometry
import Computation
import BaseClasses(Widget)
import Packer


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>Space</code> datatype.
data Space = Space Distance Frame


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

---
-- Constructs a new space widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param dist    - the horizontal or vertical distance (depending on the
--                  space widget's orientation).
-- @param cnf     - the list of configuration options for this
--                  space widget.
-- @return result - A space widget.
newSpace :: Container par => par -> Distance -> [Config Space] -> IO Space
newSpace par dist cnf =
  do
    f <- newFrame par []
    configure (Space dist f) (defaults : cnf)
  where defaults = orient Vertical


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance Eq Space where
---
-- Internal.
  (Space _ f1) == (Space _ f2) = f1 == f2

---
-- Internal.
instance GUIObject Space where 
---
-- Internal.
  toGUIObject (Space d f) = toGUIObject f
---
-- Internal.
  cname _ = "Space"

---
-- A space widget can be destroyed.
instance Destroyable Space where
---
-- Destroys a space widget.
  destroy   = destroy . toGUIObject

---
-- A radiobutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Space

---
-- You can synchronize on a space widget.
instance Synchronized Space where
---
-- Synchronizes on a space widget.
  synchronize w = synchronize (toGUIObject w)

---
-- A space widget has a configureable background colour.
instance HasColour Space where
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- The space widgets orientation can either be <code>vertical</code> or
-- <code>Horizontal</code>.
instance HasOrientation Space where
---
-- Sets the orientation of the space widget.
  orient or s @ (Space d f) =
    configure f (case or of Horizontal -> [{-fill Vertical,-} width d,
                                           height 0]
                            Vertical -> [{-fill Horizontal,-} height d,
                                         width 0]) >> 
    return s

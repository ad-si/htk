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
-- HTk's <strong>label</strong> widget.<br>
-- A label is a simple container for text or images/bitmaps.
module Label (

  Label,
  newLabel

) where

import Core
import BaseClasses(Widget)
import Configuration
import Image
import BitMap
import Computation
import Destructible
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>Label</code> datatype.
newtype Label = Label GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new label widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this label.
-- @return result - A label widget.
newLabel :: Container par => par -> [Config Label] -> IO Label
newLabel par cnf =
  do
    w <- createWidget (toGUIObject par) LABEL
    configure (Label w) cnf


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Label where 
---
-- Internal.
  toGUIObject (Label w) = w
---
-- Internal.
  cname _ = "Label"

---
-- A label widget can be destroyed.
instance Destroyable Label where
---
-- Destroys a label widget.
  destroy   = destroy . toGUIObject

---
-- A label widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Label

---
-- A label widget has a configureable border.
instance HasBorder Label

---
-- A label widget has a foreground and background colour.
instance HasColour Label where 
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- You can specify the font of a label.
instance HasFont Label

---
-- A label has a configureable text justification.
instance HasJustify Label

---
-- A label can contain an image.
instance HasPhoto Label

---
-- A label can contain a bitmap.
instance HasBitMap Label

---
-- You can specify the size of a label.
instance HasSize Label

---
-- You can set the index of a text character to underline.
instance HasUnderline Label

---
-- A label can contain text.
instance GUIValue b => HasText Label b

---
-- A label widget can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip Label

---
-- You can synchronize on a label object.
instance Synchronized Label where
---
-- Synchronizes on a label object.
  synchronize = synchronize . toGUIObject

---
-- A label has a text anchor.
instance HasAnchor Label

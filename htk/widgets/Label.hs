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
-- HTk's <strong>label widget</strong>.<br>
-- A label is a simple container for text or images.
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
-- type Label 
-- -----------------------------------------------------------------------

---
-- The <code>Label</code> datatype - a <code>Label String</code> contains
-- text, a <code>Label Image</code> contains an image.
newtype Label a = Label GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new label widget and returns a handler as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this label.
-- @return result - A label widget.
newLabel :: Container par => par -> [Config (Label a)] -> IO (Label a)
newLabel par cnf =
  do
    w <- createWidget (toGUIObject par) LABEL
    configure (Label w) cnf


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (Label a) where 
---
-- Internal.
  toGUIObject (Label w) = w
---
-- Internal.
  cname _ = "Label"

---
-- A label widget can be destroyed.
instance Destroyable (Label a) where
---
-- Destroys a label widget.
  destroy   = destroy . toGUIObject

---
-- A label widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (Label a)

---
-- A label widget has a configureable border.
instance HasBorder (Label a)

---
-- A label widget has a foreground and background colour.
instance HasColour (Label a) where 
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- You can specify the font of a label.
instance HasFont (Label a)

---
-- A label has a text justification configuration.
instance HasJustify (Label a)

---
-- A label can contain an image.
instance HasPhoto (Label Image)

---
-- You can specify the size of a label.
instance HasSize (Label a)

---
-- A label can contain underlined text.
instance HasUnderline (Label a)

---
-- A label can contain text.
instance GUIValue b => HasText (Label a) b

---
-- A label widget can have a tooltip.
instance HasTooltip (Label a)

---
-- You can synchronize on a label object (in JAVA style).
instance Synchronized (Label a) where
---
-- Synchronizes on a label object.
  synchronize = synchronize . toGUIObject

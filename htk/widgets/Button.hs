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
-- HTk's <strong>button widget</strong>.<br>
-- A simple click button.
module Button (

  Button,
  newButton

) where

import Core
import ButtonWidget
import BaseClasses(Widget)
import Configuration
import Image
import BitMap
import Concurrent
import Computation
import Destructible
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

---
-- The <code>Button</code> datatype.
newtype Button = Button GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new button widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this button.
-- @return result - A button widget.
newButton :: Container par => par -> [Config Button] -> IO Button
newButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) BUTTON defMethods
    configure (Button b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Button where 
---
-- Internal.
  toGUIObject (Button w) = w
---
-- Internal.
  cname _ = "Button"

---
-- A button widget can be destroyed.
instance Destroyable Button where
---
-- Destroys a button widget.
  destroy = destroy . toGUIObject

---
-- A button widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Button

---
-- A button widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event).
instance ButtonWidget Button

---
-- A button widget can contain a bitmap.
instance HasBitMap Button

---
-- A button widget has a configureable border.
instance HasBorder Button

---
-- A button widget has a normal foreground and background colour and an
-- active/disabled foreground and background colour.
instance HasColour Button where
---
-- Internal.
  legalColourID = buttonColours

---
-- A button widget is a stateful widget, it can be enabled or disabled.
instance HasEnable Button

---
-- You can specify the font of a button.
instance HasFont Button

---
-- A button has a configureable text justification.
instance HasJustify Button

---
-- A button can contain an image.
instance HasPhoto Button

---
-- You can specify the size of a button.
instance HasSize Button

---
-- A button can contain text.
instance GUIValue v => HasText Button v

---
-- You can set the index of a text character to underline.
instance HasUnderline Button

---
-- You can synchronize on a button object.
instance Synchronized Button where
---
-- Synchronizes on a button object.
  synchronize = synchronize . toGUIObject

---
-- When a button is clicked, a corresponding event is invoked.
instance HasCommand Button

---
-- A button can have a tooltip.
instance HasTooltip Button

---
-- A label has a text anchor.
instance HasAnchor Button

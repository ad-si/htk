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
-- The <code>Button</code> datatype - a <code>Button String</code>
-- contains text, a <code>Button Image</code> contains an image,
-- a <code>Button BitMap</code> contains a bitmap.
newtype Button a = Button GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new button widget and returns it as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this button.
-- @return result - A button widget.
newButton :: Container par => par -> [Config (Button a)] -> IO (Button a)
newButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) BUTTON defMethods
    configure (Button b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (Button a) where 
---
-- Internal.
  toGUIObject (Button w) = w
---
-- Internal.
  cname _ = "Button"

---
-- A button widget can be destroyed.
instance Destroyable (Button a) where
---
-- Destroys a button widget.
  destroy   = destroy . toGUIObject

---
-- A button widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (Button a)

---
-- A button widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (associated event).
instance ButtonWidget (Button a)

---
-- A button widget can contain a bitmap.
instance HasBitMap (Button BitMap)

---
-- A button widget has a configureable border.
instance HasBorder (Button a)

---
-- A button widget has a foreground and background colour.
instance HasColour (Button a) where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A button widget is a stateful widget, it can be enabled or disabled.
instance HasEnable (Button a)

---
-- You can specify the font of a button.
instance HasFont (Button String)

---
-- A button has a text justification configuration.
instance HasJustify (Button a)

---
-- A button can contain an image.
instance HasPhoto (Button Image)

---
-- You can specify the size of a button.
instance HasSize (Button a)

---
-- A button can contain text.
instance GUIValue v => HasText (Button String) v

---
-- A button can contain underlined text.
instance HasUnderline (Button String)

---
-- You can synchronize on a button object (in JAVA style).
instance Synchronized (Button a) where
---
-- Synchronizes on a button object.
  synchronize = synchronize . toGUIObject

---
-- When a button is clicked, a corresponding event is invoked.
instance HasCommand (Button a)

---
-- A button can have a tooltip.
instance HasTooltip (Button a)

---
-- A label has an anchor.
instance HasAnchor (Button a)
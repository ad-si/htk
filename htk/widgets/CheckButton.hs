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
-- HTk's <strong>check button widget</strong>.<br>
-- A simple check button which corresponds to a polymorphic variable.
module CheckButton (

  CheckButton,
  newCheckButton

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
import TkVariables
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

---
-- The <code>CheckButton</code> datatpe - it is associated with a
-- polymorphic <code>TkVariable</code> (first parameter); a
-- <code>CheckButton a String</code> contains text, a
-- <code>CheckButton a Image</code> contains an image,
-- <code>CheckButton a Bitmap</code> contains a bitmap.
newtype CheckButton a b = CheckButton GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new check button widget and returns it as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this check
--                  button.
-- @return result - A check button widget.
newCheckButton :: Container par => par -> [Config (CheckButton a b)] ->
                                   IO (CheckButton a b)
newCheckButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) CHECKBUTTON defMethods
    configure (CheckButton b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (CheckButton a b) where 
---
-- Internal.
  toGUIObject (CheckButton w) = w
---
-- Internal.
  cname _ = "CheckButton"

---
-- A check button widget can be destroyed.
instance Destroyable (CheckButton a b) where
---
-- Destroys a check button widget.
  destroy   = destroy . toGUIObject

---
-- A check button widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (CheckButton a b)

---
-- A check button widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (associated event) as any button
-- widget.
instance ButtonWidget (CheckButton a b)

---
-- A check button widget can contain a bitmap.
instance HasBitMap (CheckButton a BitMap)

---
-- A check button widget has a configureable border.
instance HasBorder (CheckButton a b)

---
-- A check button widget has a foreground and background colour.
instance HasColour (CheckButton a b) where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A check button widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable (CheckButton a b)

---
-- You can specify the font of a check button.
instance HasFont (CheckButton a String)

---
-- A check button has a text justification configuration.
instance HasJustify (CheckButton a String)

---
-- A check button can contain an image.
instance HasPhoto (CheckButton a Image)

---
-- You can specify the size of a check button.
instance HasSize (CheckButton a b)

---
-- A check button can contain text.
instance GUIValue v => HasText (CheckButton a String) v

---
-- A check button can contain underlined text.
instance HasUnderline (CheckButton a String)

---
-- You can synchronize on a check button object (in JAVA style).
instance Synchronized (CheckButton a b) where
  synchronize = synchronize . toGUIObject

---
-- When a check button is clicked, a corresponding event is invoked.
instance HasCommand (CheckButton a b)

---
-- A checkbutton has a value, which corresponds to a polymorphic
-- <code>TkVariable</code>.
instance GUIValue a => HasValue (CheckButton a b) a

---
-- The polymorphic variable, to which the check button's value
-- corresponds.
instance HasVariable (CheckButton a b) {-(TkVariable a)-}

---
-- An checkbutton can have a tooltip.
instance HasTooltip (CheckButton a b)

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
-- HTk's <strong>radiobutton</strong> widget.<br>
-- A simple radiobutton associated with a polymorphic variable.
module RadioButton (

  ButtonWidget(..),

  RadioButton,
  newRadioButton

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
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>RadioButton</code> datatpe - it is associated with a
-- polymorphic <code>TkVariable</code> (first type parameter); a
-- <code>RadioButton a String</code> contains text, a
-- <code>RadioButton a Image</code> contains an image,
-- <code>RadioButton a BitMap</code> contains a bitmap.
newtype RadioButton a b = RadioButton GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new radiobutton widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this
--                  radiobutton.
-- @return result - A radiobutton widget.
newRadioButton :: Container par => par -> [Config (RadioButton a b)] ->
                                   IO (RadioButton a b)
newRadioButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) RADIOBUTTON defMethods
    configure (RadioButton b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (RadioButton a b) where 
---
-- Internal.
  toGUIObject (RadioButton w) = w
---
-- Internal.
  cname _ = "RadioButton"

---
-- A radiobutton widget can be destroyed.
instance Destroyable (RadioButton a b) where
---
-- Destroys a radiobutton widget.
  destroy = destroy . toGUIObject

---
-- A radiobutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (RadioButton a b)
 
---
-- A radiobutton widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event) as any button
-- widget.
instance ButtonWidget (RadioButton a b)

---
-- A radiobutton widget can contain a bitmap.
instance HasBitMap (RadioButton a BitMap)

---
-- A radiobutton widget has a configureable border.
instance HasBorder (RadioButton a b)

---
-- A radiobutton widget has a normal foreground and background colour and
-- an active/disabled foreground and background colour.
instance HasColour (RadioButton a b) where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A radiobutton widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable (RadioButton a b)

---
-- You can specify the font of a check button.
instance HasFont (RadioButton a String)

---
-- A radiobutton has a configureable text justification.
instance HasJustify (RadioButton a String)

---
-- A radiobutton can contain an image.
instance HasPhoto (RadioButton a Image)

---
-- You can specify the size of a radiobutton.
instance HasSize (RadioButton a b)

---
-- A radiobutton can contain text.
instance GUIValue v => HasText (RadioButton a String) v

---
-- You can set the index of a text character to underline.
instance HasUnderline (RadioButton a String)

---
-- You can synchronize on a radiobutton object.
instance Synchronized (RadioButton a b) where
---
-- Synchronizes on a radiobutton object.
  synchronize = synchronize . toGUIObject

---
-- When a radiobutton is clicked, a corresponding event is invoked.
instance HasCommand (RadioButton a b)

---
-- A radiobutton has a value, that corresponds to a polymorphic
-- <code>TkVariable</code>.
instance GUIValue c => HasValue (RadioButton a b) c

---
-- The radiobutton's value is associated with a polymorphic
-- <code>TkVariable</code>.
instance HasVariable (RadioButton a b)

---
-- A radiobutton can have a tooltip.
instance HasTooltip (RadioButton a b)

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
-- HTk's <strong>checkbutton</strong> widget.<br>
-- A simple checkbutton associated with a polymorphic variable.
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
-- polymorphic <code>TkVariable</code>.
newtype CheckButton a = CheckButton GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new checkbutton widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this
--                  checkbutton.
-- @return result - A checkbutton widget.
newCheckButton :: Container par => par -> [Config (CheckButton a)] ->
                                   IO (CheckButton a)
newCheckButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) CHECKBUTTON defMethods
    configure (CheckButton b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (CheckButton a) where 
---
-- Internal.
  toGUIObject (CheckButton w) = w
---
-- Internal.
  cname _ = "CheckButton"

---
-- A checkbutton widget can be destroyed.
instance Destroyable (CheckButton a) where
---
-- Destroys a checkbutton widget.
  destroy   = destroy . toGUIObject

---
-- A checkbutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (CheckButton a)

---
-- A checkbutton widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event) as any button
-- widget.
instance ButtonWidget (CheckButton a)

---
-- A checkbutton widget can contain a bitmap.
instance HasBitMap (CheckButton a)

---
-- A checkbutton widget has a configureable border.
instance HasBorder (CheckButton a)

---
-- A checkbutton widget has a normal foreground and background colour and
-- an active/disabled foreground and background colour.
instance HasColour (CheckButton a) where 
---
-- Internal.
  legalColourID = buttonColours

---
-- A checkbutton widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable (CheckButton a)

---
-- You can specify the font of a checkbutton.
instance HasFont (CheckButton a)

---
-- A checkbutton has a text justification configuration.
instance HasJustify (CheckButton a)

---
-- A checkbutton can contain an image.
instance HasPhoto (CheckButton a)

---
-- You can specify the size of a checkbutton.
instance HasSize (CheckButton a)

---
-- A checkbutton can contain text.
instance GUIValue v => HasText (CheckButton a) v

---
-- You can set the index of a text character to underline.
instance HasUnderline (CheckButton a)

---
-- You can synchronize on a checkbutton object.
instance Synchronized (CheckButton a) where
---
-- Synchronizes on a checkbutton object.
  synchronize = synchronize . toGUIObject

---
-- When a checkbutton is clicked, a corresponding event is invoked.
instance HasCommand (CheckButton a)

---
-- A checkbutton has a value, that corresponds to a polymorphic
-- <code>TkVariable</code>.  
-- instance GUIValue a => HasValue (CheckButton a) a
-- No, it doesn't.

---
-- The polymorphic variable the checkbutton's value is associated with.
instance HasVariable (CheckButton a)

---
-- An checkbutton can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip (CheckButton a)

---
-- A checkbutton has a text anchor.
instance HasAnchor (CheckButton a)

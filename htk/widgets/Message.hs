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
-- HTk's <strong>message widget</strong>.<br>
-- A message widget is a simple container for text.
module Message (

  Message,
  newMessage,

  aspect,
  getAspect

) where

import Core
import BaseClasses(Widget)
import Configuration
import Destructible
import Computation
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

---
-- The <code>Message</code> datatype.
newtype Message = Message GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

---
-- Constructs a new message widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this message
--                  widget.
-- @return result - A message widget.
newMessage :: Container par => par -> [Config Message] -> IO Message
newMessage par cnf =
  do
    w <- createWidget (toGUIObject par) MESSAGE
    configure (Message w) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Message where 
---
-- Internal.
  toGUIObject (Message w) = w
---
-- Internal.
  cname _ = "Message"

---
-- A message widget can be destroyed.
instance Destroyable Message where
---
-- Destroys a message widget.
  destroy   = destroy . toGUIObject

---
-- A message widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Message

---
-- A message widget has a configureable border.
instance HasBorder Message

---
-- A message widget has a foreground and background colour.
instance HasColour Message where
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- You can specify the font of a message widget.
instance HasFont Message

---
-- A message widget has a configureable text justification.
instance HasJustify Message

---
-- You can specify the width of a message widget (height configuration
-- is ignored).
instance HasSize Message where
  height _ w = return w
  getHeight _ = return 1

---
-- A message widget can contain text.
instance GUIValue b => HasText Message b where
---
-- Sets the text of the message widget.
-- @param t	  - the text to set.
-- @param w	  - the concerned message widget.
-- @return result - The concerned message widget.
  text t w   = cset w "text" t
---
-- Gets the text from a message widget.
-- @param w	  - the concerned message widget.
-- @return result - the set text.
  getText w  = cget w "text"

---
-- You can synchronize on a message object (in JAVA style).
instance Synchronized Message where
---
-- Synchronizes on a message object.
  synchronize = synchronize . toGUIObject

---
-- A message widget can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip Message

---
-- An message widget has a text anchor.
instance HasAnchor Message


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

---
-- Sets the aspect of a message widget (100 * width / height).
-- @param i	  - the aspect to set.
-- @param mes	  - the concerned message widget.
-- @return result - The concerned message widget.
aspect :: Int -> Config Message
aspect i mes = cset mes "aspect" i

---
-- Gets the aspect froma message widget.
-- @param mes	  - the concerned message widget.
-- @return result - The current aspect of this message widget.
getAspect :: Message -> IO Int
getAspect mes = cget mes "aspect"

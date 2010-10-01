{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk's <strong>message widget</strong>.<br>
-- A message widget is a simple container for text.
module HTk.Widgets.Message (

  Message,
  newMessage,

  aspect,
  getAspect

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import Events.Destructible
import Util.Computation
import Events.Synchronized
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

-- | The @Message@ datatype.
newtype Message = Message GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Constructs a new message widget and returns a handler.
newMessage :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Message]
   -- ^ the list of configuration options for this message
   -- widget.
   -> IO Message
   -- ^ A message widget.
newMessage par cnf =
  do
    w <- createWidget (toGUIObject par) MESSAGE
    configure (Message w) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Message where
  toGUIObject (Message w) = w
  cname _ = "Message"

-- | A message widget can be destroyed.
instance Destroyable Message where
  destroy   = destroy . toGUIObject

-- | A message widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Message

-- | A message widget has a configureable border.
instance HasBorder Message

-- | A message widget has a foreground and background colour.
instance HasColour Message where
  legalColourID = hasForeGroundColour

-- | You can specify the font of a message widget.
instance HasFont Message

-- | A message widget has a configureable text justification.
instance HasJustify Message

-- | You can specify the width of a message widget (height configuration
-- is ignored).
instance HasSize Message where
  height _ w = return w
  getHeight _ = return 1

-- | A message widget can contain text.
instance GUIValue b => HasText Message b where
  text t w   = cset w "text" t

  -- Gets the text from a message widget.
  --    w         - the concerned message widget.
  --    result    - the set text.
  getText w  = cget w "text"

-- | You can synchronize on a message object (in JAVA style).
instance Synchronized Message where
  synchronize = synchronize . toGUIObject

-- | A message widget can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip Message

-- | An message widget has a text anchor.
instance HasAnchor Message


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

-- | Sets the aspect of a message widget (100 \* width \/ height).
aspect :: Int -> Config Message
aspect i mes = cset mes "aspect" i

-- | Gets the aspect froma message widget.
getAspect :: Message
   -- ^ the concerned message widget.
   -> IO Int
   -- ^ The current aspect of this message widget.
getAspect mes = cget mes "aspect"

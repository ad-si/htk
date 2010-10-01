{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /button widget/.
-- A simple click button.
module HTk.Widgets.Button (

  Button,
  newButton

) where

import Util.Computation
import Events.Destructible
import Events.Synchronized

import HTk.Kernel.Core
import HTk.Kernel.ButtonWidget
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Components.Image
import HTk.Components.BitMap
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

-- | The @Button@ datatype.
newtype Button = Button GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new button widget and returns a handler.
newButton :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Button]
   -- ^ the list of configuration options for this button.
   -> IO Button
   -- ^ A button widget.
newButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) BUTTON defMethods
    configure (Button b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Button where
  toGUIObject (Button w) = w
  cname _ = "Button"

-- | A button widget can be destroyed.
instance Destroyable Button where
  --  Destroys a button widget.
  destroy = destroy . toGUIObject

-- | A button widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Button

-- | A button widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event).
instance ButtonWidget Button

-- | A button widget can contain a bitmap.
instance HasBitMap Button

-- | A button widget has a configureable border.
instance HasBorder Button

-- | A button widget has a normal foreground and background colour and an
-- active\/disabled foreground and background colour.
instance HasColour Button where
  legalColourID = buttonColours

-- | A button widget is a stateful widget, it can be enabled or disabled.
instance HasEnable Button

-- | You can specify the font of a button.
instance HasFont Button

-- | A button has a configureable text justification.
instance HasJustify Button

-- | A button can contain an image.
instance HasPhoto Button

-- | You can specify the size of a button.
instance HasSize Button

-- | A button can contain text.
instance GUIValue v => HasText Button v

-- | You can set the index of a text character to underline.
instance HasUnderline Button

-- | You can synchronize on a button object.
instance Synchronized Button where
  --  Synchronizes on a button object.
  synchronize = synchronize . toGUIObject

-- | When a button is clicked, a corresponding event is invoked.
instance HasCommand Button

-- | A button can have a tooltip.
instance HasTooltip Button

-- | A button has a text anchor.
instance HasAnchor Button

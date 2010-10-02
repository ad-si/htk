{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /radiobutton/ widget.
-- A simple radiobutton associated with a polymorphic variable.
module HTk.Widgets.RadioButton (

  RadioButton,
  newRadioButton

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
import HTk.Kernel.TkVariables
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @RadioButton@ datatpe - it is associated with a
-- polymorphic @TkVariable@.
newtype RadioButton a = RadioButton GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new radiobutton widget and returns a handler.
newRadioButton :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config (RadioButton a)]
   -- ^ the list of configuration options for this
   -- radiobutton.
   ->
   IO (RadioButton a)
   -- ^ A radiobutton widget.
newRadioButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) RADIOBUTTON defMethods
    configure (RadioButton b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject (RadioButton a) where
  toGUIObject (RadioButton w) = w
  cname _ = "RadioButton"

-- | A radiobutton widget can be destroyed.
instance Destroyable (RadioButton a) where
  --  Destroys a radiobutton widget.
  destroy = destroy . toGUIObject

-- | A radiobutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (RadioButton a)

-- | A radiobutton widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event) as any button
-- widget.
instance ButtonWidget (RadioButton a)

-- | A radiobutton widget can contain a bitmap.
instance HasBitMap (RadioButton a)

-- | A radiobutton widget has a configureable border.
instance HasBorder (RadioButton a)

-- | A radiobutton widget has a normal foreground and background colour and
-- an active\/disabled foreground and background colour.
instance HasColour (RadioButton a) where
  legalColourID = buttonColours

-- | A radiobutton widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable (RadioButton a)

-- | You can specify the font of a check button.
instance HasFont (RadioButton a)

-- | A radiobutton has a configureable text justification.
instance HasJustify (RadioButton a)

-- | A radiobutton can contain an image.
instance HasPhoto (RadioButton a)

-- | You can specify the size of a radiobutton.
instance HasSize (RadioButton a)

-- | A radiobutton can contain text.
instance GUIValue v => HasText (RadioButton a) v

-- | You can set the index of a text character to underline.
instance HasUnderline (RadioButton a)

-- | You can synchronize on a radiobutton object.
instance Synchronized (RadioButton a) where
  --  Synchronizes on a radiobutton object.
  synchronize = synchronize . toGUIObject

-- | When a radiobutton is clicked, a corresponding event is invoked.
instance HasCommand (RadioButton a)

-- | A radiobutton has a value, that corresponds to a polymorphic
-- @TkVariable@.
instance GUIValue c => HasValue (RadioButton a) c

-- | The radiobutton\'s value is associated with a polymorphic
-- @TkVariable@.
instance HasVariable (RadioButton a)

-- | A radiobutton can have a tooltip.
instance HasTooltip (RadioButton a)

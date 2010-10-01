{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HTk\'s /label/ widget.
-- A label is a simple container for text or images\/bitmaps.
module HTk.Widgets.Label (

  Label,
  newLabel

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Components.Image
import HTk.Components.BitMap
import Util.Computation
import Events.Destructible
import Events.Synchronized
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Label@ datatype.
newtype Label = Label GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new label widget and returns a handler.
newLabel :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Label]
   -- ^ the list of configuration options for this label.
   -> IO Label
   -- ^ A label widget.
newLabel par cnf =
  do
    w <- createWidget (toGUIObject par) LABEL
    configure (Label w) cnf


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Label where
  toGUIObject (Label w) = w
  cname _ = "Label"

-- | A label widget can be destroyed.
instance Destroyable Label where
  -- Destroys a label widget.
  destroy   = destroy . toGUIObject

-- | A label widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Label

-- | A label widget has a configureable border.
instance HasBorder Label

-- | A label widget has a foreground and background colour.
instance HasColour Label where
  legalColourID = hasForeGroundColour

-- | You can specify the font of a label.
instance HasFont Label

-- | A label has a configureable text justification.
instance HasJustify Label

-- | A label can contain an image.
instance HasPhoto Label

-- | A label can contain a bitmap.
instance HasBitMap Label

-- | You can specify the size of a label.
instance HasSize Label

-- | You can set the index of a text character to underline.
instance HasUnderline Label

-- | A label can contain text.
instance GUIValue b => HasText Label b

-- | A label widget can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip Label

-- | You can synchronize on a label object.
instance Synchronized Label where
  -- Synchronizes on a label object.
  synchronize = synchronize . toGUIObject

-- | A label has a text anchor.
instance HasAnchor Label

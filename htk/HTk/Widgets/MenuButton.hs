{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HTk.Widgets.MenuButton (

  MenuButton,
  newMenuButton

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
import HTk.Menuitems.Menu
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

-- | The @MenuButton@ datatype.
data MenuButton = MenuButton GUIOBJECT


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new menubutton widget and returns a handler.
newMenuButton :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config MenuButton]
   -- ^ the list of configuration options for this menubutton.
   -> IO MenuButton
   -- ^ A menubutton widget.
newMenuButton par cnf =
  do
    b <- createGUIObject (toGUIObject par) MENUBUTTON defMethods
    configure (MenuButton b) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject MenuButton where
  toGUIObject (MenuButton w) = w
  cname _ = "MenuButton"

-- | A menubutton widget can be destroyed.
instance Destroyable MenuButton where
  --  Destroys a menubutton widget.
  destroy   = destroy . toGUIObject

-- | A menubutton widget has standard widget properties
-- (concerning focus, cursor).
instance Widget MenuButton

-- | A menubutton widget can be flashed (redisplayed several times in
-- alternate colours) and invoked (the associated event).
instance ButtonWidget MenuButton

-- | A menubutton widget can contain a bitmap.
instance HasBitMap MenuButton

-- | A menubutton widget has a configureable border.
instance HasBorder MenuButton

-- | A menu button has a normal foreground and background colour and an
-- active\/disabled foreground and background colour.
instance HasColour MenuButton where
  legalColourID = buttonColours

-- | A menubutton widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable MenuButton

-- | You can specify the font of a menubutton.
instance HasFont MenuButton

-- | A menu button has a configureable text justification.
instance HasJustify MenuButton

-- | A menubutton can contain an image.
instance HasPhoto MenuButton

-- | You can specify the size of a menubutton.
instance HasSize MenuButton

-- | A menubutton can contain text.
instance GUIValue v => HasText MenuButton v

-- | You can set the index of a text character to underline.
instance HasUnderline MenuButton

-- | You can synchronize on a menubutton object (in JAVA style).
instance Synchronized MenuButton where
  --  Synchronizes on a menubutton object.
  synchronize = synchronize . toGUIObject

-- | When a menubutton is clicked, a corresponding event is invoked.
instance HasCommand MenuButton

-- | A menubutton is a menu container.
instance HasMenu MenuButton

-- | A menubutton can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip MenuButton

-- | A menubutton has a text anchor.
instance HasAnchor MenuButton

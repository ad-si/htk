{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /menu checkbutton/.
-- A simple checkbutton inside a menu associated with a polymorphic
-- variable.
module HTk.Menuitems.MenuCheckButton (

  MenuCheckButton,
  createMenuCheckButton

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Components.BitMap
import HTk.Menuitems.Menu
import HTk.Menuitems.MenuItem
import HTk.Menuitems.Indicator
import Events.Synchronized
import Util.Computation
import HTk.Kernel.TkVariables


-- -----------------------------------------------------------------------
-- MenuCascade type
-- -----------------------------------------------------------------------

-- | The @MenuCheckButton@ datatype.
newtype MenuCheckButton = MenuCheckButton GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new menu checkbutton and returns a handler.
createMenuCheckButton :: Menu
   -- ^ the parent menu.
   -> [Config MenuCheckButton]
   -- ^ the list of configuration options for this menu
   -- checkbutton.
   ->
   IO MenuCheckButton
   -- ^ A menu checkbutton.
createMenuCheckButton m cnf =
  createMenuItem m MENUCHECKBUTTON MenuCheckButton cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A menu checkbutton is a menu item (any menu item instantiates the
-- abstract @class MenuItem@).
instance MenuItem MenuCheckButton

-- | Internal.
instance Eq MenuCheckButton where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject MenuCheckButton where
  toGUIObject (MenuCheckButton w) = w
  cname _ = "MenuCheckButton"

-- | You can synchronize on a menu checkbutton.
instance Synchronized MenuCheckButton where
  -- Synchronizes on a menu checkbutton.
  synchronize = synchronize . toGUIObject

-- | A menu checkbutton has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuCheckButton

-- | A menu checkbutton can contain a bitmap (instead of text or an image).
instance HasBitMap MenuCheckButton

-- | A menu checkbutton has a configureable border.
instance HasBorder MenuCheckButton

-- | A menu checkbutton has a normal foreground and background colour and
-- an active\/disabled foreground and background colour.
instance HasColour MenuCheckButton where
  legalColourID = buttonColours

-- | A menu checkbutton is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuCheckButton

-- | You can specify the font of a menu checkbutton.
instance HasFont MenuCheckButton

-- | A menu checkbutton has a configureable text justification.
instance HasJustify MenuCheckButton

-- | You can display an indicator with a menu checkbutton.
instance HasIndicator MenuCheckButton

-- | A menu checkbutton can contain an image (instead of text or a bitmap).
instance HasPhoto MenuCheckButton

-- | You can specify the size of a menu checkbutton.
instance HasSize MenuCheckButton

-- | A menu checkbutton can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuCheckButton v where
  -- Sets the text to display.
  text str w = cset w "label" str >> return w
  -- Gets the displayed text.
  getText w = cget w "label"

-- | You can set the index of a text character to underline.
instance HasUnderline MenuCheckButton

-- | The polymorphic variable the menu checkbutton\'s value is associated
-- with.
instance HasVariable MenuCheckButton

-- | A menu checkbutton has a value, that corresponds to a polymorphic
-- @TkVariable@.
instance GUIValue v => HasValue MenuCheckButton v

-- | When a menu checkbutton is clicked, a corresponding event is invoked.
instance HasCommand MenuCheckButton

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /menu radiobutton/.
-- A simple radiobutton inside a menu associated with a polymorphic
-- variable.
module HTk.Menuitems.MenuRadioButton (

  MenuRadioButton,
  createMenuRadioButton

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
-- datatype
-- -----------------------------------------------------------------------

-- | The @MenuRadioButton@ datatype.
newtype MenuRadioButton = MenuRadioButton GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new menu radiobutton and returns a handler.
createMenuRadioButton :: Menu
   -- ^ the parent menu.
   -> [Config MenuRadioButton]
   -- ^ the list of configuration options for this menu
   -- radiobutton.
   ->
   IO MenuRadioButton
   -- ^ A menu radiobutton.
createMenuRadioButton m cnf =
  createMenuItem m MENURADIOBUTTON MenuRadioButton cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A menu radiobutton is a menu item (any menu item instantiates the
-- abstract @class MenuItem@).
instance MenuItem MenuRadioButton

-- | Internal.
instance GUIObject MenuRadioButton where
  toGUIObject (MenuRadioButton w) = w
  cname _ = "MenuRadioButton"

-- | You can synchronize on a menu radiobutton.
instance Synchronized MenuRadioButton where
  -- Synchronizes on a menu radiobutton.
  synchronize = synchronize . toGUIObject

-- | A menu radiobutton has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuRadioButton

-- | A menu radiobutton can contain a bitmap (instead of text or an image).
instance HasBitMap MenuRadioButton

-- | A menu radiobutton has a configureable border.
instance HasBorder MenuRadioButton

-- | A menu radiobutton has a normal foreground and background colour and
-- an active\/disabled foreground and background colour.
instance HasColour MenuRadioButton where
  legalColourID = buttonColours

-- | A menu radiobutton is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuRadioButton

-- | You can specify the font of a menu radiobutton.
instance HasFont MenuRadioButton

-- | A menu radiobutton has a configureable text justification.
instance HasJustify MenuRadioButton

-- | You can display an indicator with a menu radiobutton.
instance HasIndicator MenuRadioButton

-- | You can specify the size of a menu radiobutton.
instance HasPhoto MenuRadioButton

-- | You can specify the size of a menu radiobutton.
instance HasSize MenuRadioButton

-- | A menu radiobutton can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuRadioButton v where
  -- Sets the text to display.
  text str w = cset w "label" str >> return w
  -- Gets the displayed text.
  getText w = cget w "label"

-- | You can set the index of a text character to underline.
instance HasUnderline MenuRadioButton

-- | The polymorphic variable the menu radiobutton\'s value is associated
-- with.
instance HasVariable MenuRadioButton

-- | A menu radiobutton has a value, that corresponds to a polymorphic
-- @TkVariable@.
instance GUIValue v => HasValue MenuRadioButton v

-- | When a menu radiobutton is clicked, a corresponding event is invoked.
instance HasCommand MenuRadioButton

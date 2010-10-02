{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /menu cascade item/.
-- A containers for cascaded menus.
module HTk.Menuitems.MenuCascade (

  MenuCascade,
  createMenuCascade,
  createPulldownMenu

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Components.BitMap
import HTk.Menuitems.Menu
import HTk.Menuitems.MenuItem
import HTk.Menuitems.Indicator
import Events.Synchronized
import Util.Computation


-- -----------------------------------------------------------------------
-- MenuCascade type
-- -----------------------------------------------------------------------

-- | The @MenuCascade@ datatype.
newtype MenuCascade = MenuCascade GUIOBJECT


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A @MenuCascade@ item is a container for a sub-menu.
instance HasMenu MenuCascade


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new menu cascasde item and returns a handler.
createMenuCascade :: Menu
   -- ^ the parent menu.
   -> [Config MenuCascade]
   -- ^ the list of configuration options for this menu
   -- cascade item.
   -> IO MenuCascade
   -- ^ A menu cascade item.
createMenuCascade m cnf = createMenuItem m MENUCASCADE MenuCascade cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | A menu cascade item is a menu item (any menu item instantiates the
-- abstract @class MenuItem@).
instance MenuItem MenuCascade

-- | Internal.
instance Eq MenuCascade where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject MenuCascade where
  toGUIObject (MenuCascade w) = w
  cname _ = "MenuCascade"

-- | You can synchronize on a menu cascade item.
instance Synchronized MenuCascade where
  -- Synchronizes on a menu cascade item.
  synchronize = synchronize . toGUIObject

-- | A menu cascade item has an optional text to display as a reminder
-- about a keystroke binding.
instance HasAccelerator MenuCascade

-- | A menu cascade item can contain a bitmap (instead of text or an image).
instance HasBitMap MenuCascade

-- | A menu cascade item has a configureable border.
instance HasBorder MenuCascade

-- | A menu cascade item has a normal foreground and background colour and
-- an active\/disabled foreground and background colour.
instance HasColour MenuCascade where
  legalColourID = buttonColours

-- | A menu cascade item is a stateful object, it can be enabled or
-- disabled.
instance HasEnable MenuCascade

-- | You can specify the font of a menu cascade item.
instance HasFont MenuCascade

-- | A menu cascade item has a configureable text justification.
instance HasJustify MenuCascade

-- | You can display an indicator with a menu cascade item.
instance HasIndicator MenuCascade

-- | A menu cascade item can contain an image (instead of text or a bitmap).
instance HasPhoto MenuCascade

-- | You can specify the size of a menu cascade item.
instance HasSize MenuCascade

-- | A menu cascade item can contain text (instead of an image or bitmap).
instance GUIValue v => HasText MenuCascade v where
  -- Sets the text to display.
  text str w = cset w "label" str >> return w
  -- Gets the displayed text.
  getText w = cget w "label"

-- | You can set the index of a text character to underline.
instance HasUnderline MenuCascade


-- | Utility function: create a pulldown menu
--
createPulldownMenu :: Menu
   -- ^ the parent menu.
   -> [Config MenuCascade]
   -- ^ the list of configuration options for this pulldown menu
   -> IO Menu
   -- ^ A menu cascade item.
createPulldownMenu mpar conf =
  do pd <- createMenuCascade mpar conf
     m  <- createMenu mpar False []
     pd # menu m
     return m


{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides access to a widgets selection (e.g. inside a
-- listbox, editor or entry widget).
module HTk.Components.Selection (
  Selection(..),
  HasSelection(..),
  HasSelectionIndex(..),
  HasSelectionBaseIndex(..),
  HasSelectionIndexRange(..),
  HasSelectionBaseIndexRange(..)

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Colour(toColour)
import Util.Computation


-- -----------------------------------------------------------------------
-- selection classes
-- -----------------------------------------------------------------------

-- | A widget with a selectable content instantiates the @class
-- HasSelection@.
class GUIObject w => HasSelection w where
  -- Clears the widgets selection.
  clearSelection    :: w -> IO ()

-- | A widget with a indexable selection instantiates the @class
-- HasSelectionIndex@.
class HasSelectionIndex w i where
  -- Selects the entry at the specified index.
  selection         :: i -> Config w
  -- Queries if the entry at the given index is selected.
  isSelected        :: w -> i -> IO Bool

-- | A widget with an indexable selection base instantiates the @class
-- HasSelectionBaseIndex@.
class HasSelectionBaseIndex w i where
  -- Gets the selected base index (if something is selected).
  getSelection :: w -> IO (Maybe i)

-- | A widget with an indexable selection range instantiates the @class
-- HasSelectionIndexRange@.
class HasSelectionIndexRange w i1 i2 where
  -- Selects the widget\'s entries in the specified range.
  selectionRange    :: i1 -> i2 -> Config w

-- | A widget with an indexable selection index range instantiates the
-- @class HasSelectionBaseIndexRange@.
class HasSelectionIndex w i => HasSelectionBaseIndexRange w i where
  -- Gets the selection start index.
  getSelectionStart :: w -> IO (Maybe i)
  -- Gets the selection end index.
  getSelectionEnd   :: w -> IO (Maybe i)
  -- Gets the selection range.
  getSelectionRange :: w -> IO (Maybe (i,i))
  getSelectionRange w =
    do
      start <- getSelectionStart w
      end <- getSelectionEnd w
      case (start,end) of
        ((Just start), (Just end)) -> return (Just (start,end))
        _ -> return Nothing


-- -----------------------------------------------------------------------
-- handle
-- -----------------------------------------------------------------------

-- | The @Selection@ datatype.
newtype Selection w = Selection w


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject w => GUIObject (Selection w) where
  toGUIObject (Selection w) = toGUIObject w
  cname (Selection w)       = cname w

-- | The selected entries have a configureable foreground and background
-- colour.
instance (HasSelection w,Widget w) => HasColour (Selection w) where
  legalColourID = hasForeGroundColour
  setColour w "background" c = cset w "selectbackground" (toColour c)
  setColour w "foreground" c = cset w "selectforeground" (toColour c)
  setColour w _ _            = return w
  getColour w "background"   = cget w "selectbackground"
  getColour w "foreground"   = cget w "selectforeground"
  getColour _ _              = return cdefault

-- | The selection has a configureable border.
instance (HasSelection w,Widget w) => HasBorder (Selection w) where
  -- Specifies the size of the 3D border for selection highlight.
  borderwidth s w  = cset w "selectborderwidth" s
  getBorderwidth w = cget w "selectborderwidth"
  -- Dummy.
  relief  _ w      = return w
  getRelief _      = return Raised

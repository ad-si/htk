{-# LANGUAGE MultiParamTypeClasses #-}

-- | Basic types and classes concerning insertion cursors in entry and
-- text fields.
module HTk.Components.ICursor (
  ICursor(..),
  HasInsertionCursor,
  HasInsertionCursorIndexGet(..),
  HasInsertionCursorIndexSet(..),

  insertOffTime,
  getInsertOffTime,

  insertOnTime,
  getInsertOnTime

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Colour(toColour)
import Util.Computation


-- -----------------------------------------------------------------------
-- classes for insertion cursor
-- -----------------------------------------------------------------------

-- | Widgets with an insertion cursor instantiate the
-- @class HasInsertionCursor@.
class Widget w => HasInsertionCursor w

-- | Widgets with an insertion cursor that can be set to a specific index
-- instantiate the @class HasInsertionCursorIndexSet@.
class HasInsertionCursor w => HasInsertionCursorIndexSet w i where
  -- Sets the index of the insertion Cursor.
  insertionCursor :: i -> Config w

-- | Widgets from which you can get the index of the insertion cursor
-- instantiate the @class HasInsertionCursorIndexSet@.
class HasInsertionCursor w => HasInsertionCursorIndexGet w i where
  getInsertionCursor :: w -> IO i


-- -----------------------------------------------------------------------
-- handle
-- -----------------------------------------------------------------------

-- | The @ICursor@ datatype.
newtype ICursor w = ICursor w


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject w => GUIObject (ICursor w) where
  toGUIObject (ICursor w) = toGUIObject w
  cname (ICursor w) = cname w

-- | The insertion cursor has a configureable colour.
instance (HasInsertionCursor w,Widget w) => HasColour (ICursor w) where
  legalColourID = hasBackGroundColour
  setColour w "bg" c = cset w "insertbackground" (toColour c)
  setColour w _ _ = return w
  getColour w "bg" = cget w "insertbackground"
  getColour _ _ = return cdefault

-- | The insertion cursor has a configureable borderwidth (width for three
-- dimensional appearence).
instance (HasInsertionCursor w,Widget w) => HasBorder (ICursor w) where
  -- Sets the insertion cursor\'s borderwidth.
  borderwidth s w = cset w "insertborderwidth" s
  -- Gets the insertion cursor\'s borderwidth.
  getBorderwidth w = cget w "insertborderwidth"
  -- Dummy.
  relief _ w = return w
  -- Dummy.
  getRelief _ = return Raised

-- | The insertion cursor has a configureable width.
instance (HasInsertionCursor w,Widget w) => HasSize (ICursor w) where
  -- Sets the width of the insertion cursor.
  width s w   = cset w "insertwidth" s
  -- Gets the width of the insertion cursor.
  getWidth w  = cget w "insertwidth"
  -- Dummy.
  height h w  = return w
  -- Dummy.
  getHeight w = return cdefault


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

-- | Sets the time the insertion cursor blinks off (in milliseconds, zero
-- disables blinking).
insertOffTime :: HasInsertionCursor w => Int -> Config (ICursor w)
insertOffTime i w = cset w  "insertofftime" i

-- | Gets the time the insertion cursor blinks off.
getInsertOffTime :: HasInsertionCursor w => ICursor w -> IO Int
getInsertOffTime w = cget w "insertofftime"

-- | Sets the time the insertion cursor blinks on (in milliseconds).
insertOnTime :: HasInsertionCursor w => Int -> Config (ICursor w)
insertOnTime i w = cset w "insertontime" i

-- | Gets the time the insertion cursor blinks on.
getInsertOnTime :: HasInsertionCursor w => (ICursor w) -> IO Int
getInsertOnTime w = cget w "insertontime"

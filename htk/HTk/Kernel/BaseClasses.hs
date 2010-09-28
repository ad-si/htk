-- | Basic types and classes.
module HTk.Kernel.BaseClasses (

  Widget(..)

) where

import HTk.Kernel.GUIObject
import Util.Computation
import HTk.Kernel.Cursor


-- -----------------------------------------------------------------------
-- class Widget
-- -----------------------------------------------------------------------

-- | Widgets instantiate the @class Widget@.
class GUIObject w => Widget w where
  -- Sets the mouse cursor for this widget.
  cursor          :: CursorDesignator ch => ch -> Config w
  -- Gets the mouse cursor for this widget.
  getCursor       :: w -> IO Cursor
  -- If @True@ the concerned widget can take the focus.
  takeFocus       :: Bool -> Config w
  -- Gets the current setting.
  getTakeFocus    :: w -> IO Bool
  cursor s w       = cset w "cursor" (toCursor s)
  getCursor w      = cget w "cursor"
  takeFocus b w    = cset w "takefocus" b
  getTakeFocus w   = cget w "takefocus"

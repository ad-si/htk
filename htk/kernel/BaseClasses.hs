-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

---
-- Basic types and classes.
module BaseClasses (

  Widget(..)

) where

import GUIObject
import Computation
import Cursor


-- -----------------------------------------------------------------------
-- class Widget
-- -----------------------------------------------------------------------

---
-- Widgets instantiate the <code>class Widget</code>.
class GUIObject w => Widget w where
---
-- Sets the mouse cursor for this widget.
  cursor          :: CursorDesignator ch => ch -> Config w
---
-- Gets the mouse cursor for this widget.
  getCursor       :: w -> IO Cursor
---
-- If <code>True</code> the concerned widget can take the focus.
  takeFocus       :: Bool -> Config w
---
-- Gets the current setting.
  getTakeFocus    :: w -> IO Bool
  cursor s w       = cset w "cursor" (toCursor s)
  getCursor w      = cget w "cursor"
  takeFocus b w    = cset w "takefocus" b
  getTakeFocus w   = cget w "takefocus"

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


module BaseClasses (

  Widget(..),

) where

import GUIObject
import Computation
import Cursor


-- -----------------------------------------------------------------------
-- class Widget
-- -----------------------------------------------------------------------

class GUIObject w => Widget w where
  cursor          :: CursorDesignator ch => ch -> Config w
  getCursor       :: w -> IO Cursor
  takeFocus       :: Bool -> Config w
  getTakeFocus    :: w -> IO Bool
  cursor s w       = cset w "cursor" (toCursor s)
  getCursor w      = cget w "cursor"
  takeFocus b w    = cset w "takefocus" b
  getTakeFocus w   = cget w "takefocus"

-- -----------------------------------------------------------------------
-- 
-- HH  HH  TTTTTTTT  kk
-- HH  HH     TT     kk  kk
-- HH  HH     TT     kk kk
-- HHHHHH     TT     kkkk
-- HH  HH     TT     kk kk
-- HH  HH     TT     kk  kk
-- HH  HH     TT     kk   kk
--
-- (c) University of Bremen
--
-- Original author: Einar Karlsen,  
--                  University of Bremen
--                  email: ewk@informatik.uni-bremen.de
--
-- Current authors: cxl, ger, ludi
--
-- Module : BaseClasses
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

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
-- This module provides access to the X selection.
module XSelection (

  module Index,
  module Selection,

  HasXSelection(..),

  XSelection(..),

  clearXSelection,
  getXSelection,

) where

import Core
import Index
import Selection
import Screen
import Char
import Computation
import Window


-- -----------------------------------------------------------------------
-- class HasXSelection
-- -----------------------------------------------------------------------

---
-- Widgets that have an X selection instantiate the
-- <code>class HasXSelection</code>.
class HasSelection w => HasXSelection w where
---
-- Sets whether the selection should be exported or not.
  exportSelection         :: Bool -> Config w
---
-- Gets the current selection export setting.
  getExportSelection      :: w -> IO Bool
  exportSelection b w     = cset w "exportSelection" b
  getExportSelection w    = cget w "exportSelection"


-- -----------------------------------------------------------------------
-- types
-- -----------------------------------------------------------------------

---
-- The <code>XSelection</code> datatype.
data XSelection = PRIMARY | CLIPBOARD deriving (Eq, Ord, Show, Read)

type TargetType = String        -- STRING, ATOM, INTEGER ...


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIValue XSelection where
---
-- Internal.
  cdefault = PRIMARY


-- -----------------------------------------------------------------------
-- XSelection commands
-- -----------------------------------------------------------------------

---
-- Clears the X selection.
clearXSelection :: GUIObject a => Screen a -> XSelection -> IO ()
clearXSelection (Screen win) sel = 
        execMethod win (\nm  ->  ["selection clear -displayof " ++ show nm ++ " -selection " ++ show sel])

---
-- Gets the current X selection.
getXSelection :: (GUIObject a, GUIValue b) =>
                 Screen a-> XSelection -> TargetType -> IO b
getXSelection (Screen win) sel tp = 
        evalMethod win (\nm  ->  ["selection get -displayof " ++ show nm ++ " -selection " ++ show sel ++ " -type " ++ tp])

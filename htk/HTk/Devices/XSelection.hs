-- | This module provides access to the X selection.
module HTk.Devices.XSelection (
  HasXSelection(..),

  XSelection(..),

  clearXSelection,
  getXSelection,

) where

import HTk.Kernel.Core
import HTk.Components.Selection
import HTk.Devices.Screen
import Util.Computation

-- -----------------------------------------------------------------------
-- class HasXSelection
-- -----------------------------------------------------------------------

-- | Widgets that have an X selection instantiate the
-- @class HasXSelection@.
class HasSelection w => HasXSelection w where
  -- Sets whether the selection should be exported or not.
  exportSelection         :: Bool -> Config w
  -- Gets the current selection export setting.
  getExportSelection      :: w -> IO Bool
  exportSelection b w     = cset w "exportselection" b
  getExportSelection w    = cget w "exportselection"


-- -----------------------------------------------------------------------
-- types
-- -----------------------------------------------------------------------

-- | The @XSelection@ datatype.
data XSelection = PRIMARY | CLIPBOARD deriving (Eq, Ord, Show, Read)

type TargetType = String        -- STRING, ATOM, INTEGER ...


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIValue XSelection where
  cdefault = PRIMARY


-- -----------------------------------------------------------------------
-- XSelection commands
-- -----------------------------------------------------------------------

-- | Clears the X selection.
clearXSelection :: GUIObject a => Screen a -> XSelection -> IO ()
clearXSelection (Screen win) sel =
        execMethod win (\nm  ->  ["selection clear -displayof " ++ show nm ++ " -selection " ++ show sel])

-- | Gets the current X selection.
getXSelection :: (GUIObject a, GUIValue b) =>
                 Screen a-> XSelection -> TargetType -> IO b
getXSelection (Screen win) sel tp =
        evalMethod win (\nm  ->  ["selection get -displayof " ++ show nm ++ " -selection " ++ show sel ++ " -type " ++ tp])

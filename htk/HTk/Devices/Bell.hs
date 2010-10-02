-- | This module provides access to the terminal bell.
module HTk.Devices.Bell (

  ringBell,
  bell

) where

import HTk.Containers.Window
import HTk.Containers.Toplevel
import HTk.Kernel.Core


-- -----------------------------------------------------------------------
-- Ring Bell
-- -----------------------------------------------------------------------

-- | Rings the bell for the given window.
ringBell :: Window w => Maybe w -> IO ()
ringBell Nothing = execTclScript [tkRing Nothing]
ringBell (Just win) = execMethod win (\ nm -> [tkRing (Just nm)])

-- | Rings the bell.
bell :: IO ()
bell = ringBell (Nothing :: Maybe Toplevel)


-- -----------------------------------------------------------------------
-- Unparsing of Commands
-- -----------------------------------------------------------------------

tkRing :: Maybe ObjectName -> TclCmd
tkRing Nothing = "bell"
tkRing (Just win) = "bell -displayof " ++ show win
{-# INLINE tkRing #-}

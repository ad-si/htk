-- | This module provides access to the terminal bell.
module Bell (

  Window,
  ringBell,
  bell
        
) where

import Window
import Toplevel
import Core


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

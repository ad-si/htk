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

ringBell :: Window w => Maybe w -> IO ()
ringBell Nothing = execTclScript [tkRing Nothing]
ringBell (Just win) = execMethod win (\ nm -> [tkRing (Just nm)])

bell :: IO ()
bell = ringBell (Nothing :: Maybe Toplevel)


-- -----------------------------------------------------------------------
-- Unparsing of Commands
-- -----------------------------------------------------------------------

tkRing :: Maybe ObjectName -> TclCmd
tkRing Nothing = "bell"
tkRing (Just win) = "bell -displayof " ++ show win 
{-# INLINE tkRing #-}

{- #########################################################################

MODULE        : Bell
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The Bell, and how to ring it 

   ######################################################################### -}

module Bell (
        Window,
        ringBell,
        bell
        
        ) where

--import Concurrency
import GUICore
import Debug(debug)


-- --------------------------------------------------------------------------
-- Ring Bell 
-- --------------------------------------------------------------------------           
ringBell :: Maybe Window -> IO ()
ringBell Nothing = execTclScript [tkRing Nothing]
ringBell (Just win) = execMethod win (\ nm -> [tkRing (Just nm)])


bell :: IO ()
bell = ringBell Nothing


-- --------------------------------------------------------------------------
-- Unparsing of Commands
-- --------------------------------------------------------------------------

tkRing :: Maybe ObjectName -> TclCmd
tkRing Nothing = "bell"
tkRing (Just win) = "bell -displayof " ++ show win 
{-# INLINE tkRing #-}


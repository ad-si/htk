{- #########################################################################

   A simple semaphore
                
   ######################################################################### -}


module BSem (
   module Lock,
   
   BSem,
   newBSem,
   newLockedBSem
   ) where
 
import Concurrent

import Thread
import Lock

import Debug(debug)

-- --------------------------------------------------------------------------
-- Type
-- --------------------------------------------------------------------------

newtype BSem = BSem (MVar ()) deriving Eq

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Lock BSem where
   acquire (BSem sem) = takeMVar sem
   release (BSem sem) = putMVar sem ()

instance Synchronized BSem where
   synchronize (BSem sem) c = 
      do
         takeMVar sem
         ans <- try c
         putMVar sem ()
         propagate ans

-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newBSem :: IO BSem
newBSem = newMVar () >>= return . BSem

newLockedBSem   :: IO BSem
newLockedBSem = newEmptyMVar >>= return . BSem

{- #########################################################################

MODULE        : BSem
AUTHOR        : Einar W. Karlsen, 
                Walter Norzel 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2

DESCRIPTION   : The Binary Semaphore 

                
   ######################################################################### -}


module BSem (
        module Lock,

        BSem,
        newBSem,
        newLockedBSem
) where
 
import Thread
import Variable
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
        synchronize (BSem sem) c = do {
                takeMVar sem;
                ans <- try c;
                putMVar sem ();
                propagate ans
                }


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newBSem :: IO BSem
newBSem = newMVar () >>= return . BSem

newLockedBSem   :: IO BSem
newLockedBSem = newEmptyMVar >>= return . BSem


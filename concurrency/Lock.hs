{- #########################################################################

MODULE        : Lock
AUTHOR        : Einar W. Karlsen  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Lock class definition


   ######################################################################### -}


module Lock (
        Synchronized(..),
        Lock(..),
        HasTryAcquire(..),

        illegalLockRelease

        ) where

import Computation

import Debug(debug)



-- --------------------------------------------------------------------------
-- Class Semaphore
-- --------------------------------------------------------------------------

class Synchronized a where
        synchronize :: a -> IO b -> IO b


-- --------------------------------------------------------------------------
-- Class Lock
-- --------------------------------------------------------------------------

class Lock l where
        release :: l -> IO ()
        acquire :: l -> IO ()


class Lock l => HasTryAcquire l where
        tryAcquire :: l -> IO Bool


-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------

illegalLockRelease :: IOError
illegalLockRelease = userError "Lock: Illegal release operation"


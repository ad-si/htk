{- #########################################################################

MODULE        : Lock
AUTHOR        : Einar W. Karlsen  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Lock class definition

Lock is an instance of a typical thing we synchronize with.
Possible instances are BSem and Mutex.


   ######################################################################### -}


module Lock (
        Synchronized(..),
        Lock(..),
        HasTryAcquire(..), -- not used

        illegalLockRelease

        ) where

import Computation

import Debug(debug)



-- --------------------------------------------------------------------------
-- Class Semaphore
-- --------------------------------------------------------------------------

class Synchronized a where
-- acquire lock on a, and while we've got it do this action.
        synchronize :: a -> IO b -> IO b


-- --------------------------------------------------------------------------
-- Class Lock
-- --------------------------------------------------------------------------

class Lock l where
        release :: l -> IO ()
        acquire :: l -> IO ()


-- HasTryAcquire doesn't actually seem to be used anywhere.
class Lock l => HasTryAcquire l where
        tryAcquire :: l -> IO Bool


-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------

illegalLockRelease :: IOError
illegalLockRelease = userError "Lock: Illegal release operation"




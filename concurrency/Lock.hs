{- #########################################################################

MODULE        : Lock
AUTHOR        : Einar W. Karlsen  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Lock class definition

Lock is an instance of a typical thing we synchronize with.
Possible instances are BSem and Mutex.  We also provide a trivial
implementation called SimpleLock.


   ######################################################################### -}


module Lock (
        Synchronized(..),
        Lock(..),
        HasTryAcquire(..), -- not used
        illegalLockRelease,

        SimpleLock,
        newSimpleLock -- returns an unlocked SimpleLock
        ) where

import Computation

import Debug(debug,(@:))
import qualified Concurrent


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

-- --------------------------------------------------------------------------
--  Simple Locks
-- --------------------------------------------------------------------------

newtype SimpleLock = SimpleLock (Concurrent.MVar ())
-- empty == unlocked.

newSimpleLock :: IO SimpleLock
newSimpleLock = 
   do
      newMVar <- Concurrent.newMVar ()
      return(SimpleLock newMVar)

instance Lock SimpleLock where
   release (SimpleLock mv) = 
      do
         _ <- Concurrent.putMVar mv ()
         done
   acquire (SimpleLock mv) = Concurrent.takeMVar mv

instance Synchronized SimpleLock where
   synchronize lock action =
      do
         acquire lock
         result<-try(action)
         release lock
         propagate result



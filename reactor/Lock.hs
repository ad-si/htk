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
        illegalLockRelease,
        ) where

import Computation

import Debug(debug,(@:))
import qualified Concurrent

import Synchronized


-- --------------------------------------------------------------------------
-- Class Lock
-- --------------------------------------------------------------------------

class Lock l where
---
-- release a lock
   release :: l -> IO ()
---
-- acquire a lock
   acquire :: l -> IO ()


-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------

illegalLockRelease :: IOError
illegalLockRelease = userError "Lock: Illegal release operation"




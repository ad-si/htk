{- #########################################################################

MODULE        : ThreadWait
AUTHOR        : George Russell
                University of Bremen
DATE          : 1999
DESCRIPTION   : This file exists for historical reasons, because
                early versions of GHC (up to August 1999) did not
                define the primitives.  Now it is just a wrapper.

   ######################################################################### 
-}

module ThreadWait(waitForInputFd) where

import IO
import Concurrent

import Debug(debug)

import qualified Posix
import qualified PosixUtil

waitForInputFd :: Posix.Fd -> IO()
waitForInputFd fd  = threadWaitRead(PosixUtil.fdToInt fd)





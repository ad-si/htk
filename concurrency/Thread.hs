{- #########################################################################

MODULE        : Thread
AUTHOR        : Einar W. Karlsen, George 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : Threads with identity. 



   ######################################################################### -}


module Thread (
        module Computation,

        ThreadID,
        getThreadID,

        -- thread creation
        forkIO,

        -- delay thread execution
        Duration,
        mins,
        secs,   
        msecs,
        usecs,
        delay,
        after,
        every,
        
        ) 
where

import Maybes
import Concurrent
import Computation

import Debug(debug)

-- --------------------------------------------------------------------------
-- Data Types
-- --------------------------------------------------------------------------

type ThreadID = ThreadId

-- --------------------------------------------------------------------------
-- Delay Thread Execution
-- --------------------------------------------------------------------------

type Duration = Int -- time in microseconds

delay :: Duration -> IO ()
delay d = 
   if d<0
      then
         debug("Thread.delay - delay time of " ++ show d)
      else
         threadDelay d
{-# INLINE delay #-} 

after :: Duration -> IO a -> IO a
after d c = do {delay d; c}

every :: Duration -> IO a -> IO ()
every d c = forever (after d c)

mins  :: Double -> Duration
secs  :: Double -> Duration
msecs :: Double -> Duration
usecs :: Double -> Duration

usecs x = round(x)
msecs x = round(x*1000.0)
secs x  = round(x*1000000.0)
mins x  = round(x*60000000.0)

-- --------------------------------------------------------------------------
--  Thread Id
-- --------------------------------------------------------------------------

getThreadID :: IO ThreadID
getThreadID = myThreadId









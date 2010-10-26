{-# LANGUAGE CPP #-}

-- | Spawn provides an interface to Concurrent.forkIO which is supposed
-- to be implementable for both Hugs and GHC.
--
-- This is the GHC implementation.
module Events.Spawn(
   spawn -- :: IO () -> IO (IO ())
   ) where

import Control.Concurrent
import Control.Exception

-- | Do a fork, returning an action which may attempt to
-- kill the forked thread.  (Or may not . . .)
spawn :: IO () -> IO (IO ())
spawn action =
   do
      let quietAction = goesQuietly action
      threadId <- forkIO quietAction
      return (killThread threadId)


-- --------------------------------------------------------------------------
-- goesQuietly
-- --------------------------------------------------------------------------

goesQuietly :: IO () -> IO ()
goesQuietly action =
   do
      result <-
         tryJust
            (\ exception -> case fromException exception of
               Just ThreadKilled -> Just ()
               _ -> case fromException exception of
#if __GLASGOW_HASKELL__ >= 612
                 Just BlockedIndefinitelyOnMVar -> Just ()
#else
                 Just BlockedOnDeadMVar -> Just ()
#endif
                 _ -> Nothing
               )
            action
      case result of
         Left () -> return ()
         Right () -> return ()


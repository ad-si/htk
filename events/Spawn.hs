{- Spawn provides an interface to Concurrent.forkIO which is supposed
   to be implementable for both Hugs and GHC. 

   This is the GHC implementation. -}
module Spawn(
   spawn -- :: IO () -> IO (IO ())
   -- Do a fork, returning an action which may attempt to
   -- kill the forked thread.  (Or may not . . .)
   ) where

import Concurrent
import Exception

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
            (\ exception -> case exception of
               AsyncException ThreadKilled -> Just ()
               BlockedOnDeadMVar -> Just ()
               _ -> Nothing
               )
            action
      case result of
         Left () -> return ()
         Right () -> return ()
               

{- Run a list of actions simultaneously, finishing when each of the
   actions finishes, and returning their results.  So sort of like
   a concurrent sequence.

   Obviously it's rather important to be sure that all of the
   actions actually will finish!!!
   -}
module WaitOnN(
   waitOnN,
   ) where

import Concurrent


waitOnN :: [IO a] -> IO [a]
waitOnN [] = return []
waitOnN [action] = 
   do
      res <- action
      return [res]
waitOnN (first:rest) =
   do
      mVar <- newEmptyMVar 
      forkIO (
         do
            list <- waitOnN rest
            putMVar mVar list
         )
      firstRes <- first
      restRes <- takeMVar mVar
      return (firstRes:restRes)   
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Basic Thread operations.
module Util.Thread (

   ThreadId,

   -- thread creation

   forkIODebug, -- :: IO () -> IO ThreadId
      -- Try to be more helpful about catching exceptions.


   forkIOquiet,
      -- ALMOST identical with standard action.
      -- The differences are (a) that it takes an extra string argument
      -- (which goes first); (b) if the thread fails because of
      -- "BlockedOnMVar" nothing is printed, but we output a
      -- message to "debug" which includes the label.
      -- NB.  This function no longer seems to be necessary in recent
      -- versions of GHC (current is 6.02.1) so please don't use it.
   goesQuietly,
   -- :: IO () -> IO ()
   -- This wraps an action so that if killed nothing is printed and it
   -- just returns.  This is useful for Expect and other things which
   -- get rid of a redundant thread by killing it.
   -- Now changed so that it also prints nothing for BlockedOnMVar


   -- delay thread execution
   Duration,
   mins,
   secs,
   msecs,
   usecs,
   delay,
   after,
   every,

   mapMConcurrent,
   mapMConcurrent_,
      -- evaluate a list of IO actions concurrently.
   mapMConcurrentExcep,
      -- evaluate a list of IO actions concurrently, also propagating
      -- exceptions properly.
   )
where

import qualified GHC.Conc
import qualified GHC.Base

import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Int

import Util.Computation

import Util.Debug(debug)
import Util.ExtendedPrelude

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

-- --------------------------------------------------------------------------
-- forkIOSafe
-- --------------------------------------------------------------------------

forkIODebug :: IO () -> IO ThreadId
forkIODebug = forkIO . errorOurExceps

-- --------------------------------------------------------------------------
-- forkIOquiet
-- --------------------------------------------------------------------------

forkIOquiet :: String -> IO () -> IO ThreadId
forkIOquiet label action =
   do
      let
         newAction =
            do
               error <- tryJust
                  (\ exception -> case fromException exception of
#if __GLASGOW_HASKELL__ >= 612
                     Just BlockedIndefinitelyOnMVar -> Just ()
#else
                     Just BlockedOnDeadMVar -> Just ()
#endif
                     _ -> Nothing
                     )
                  action
               case error of
                  Right () -> done -- success
                  Left () ->
                     do
                        debug ("Thread.forkIOquiet: "++label)
      forkIO newAction


-- --------------------------------------------------------------------------
-- mapMConcurrent
-- --------------------------------------------------------------------------

mapMConcurrent :: (a -> IO b) -> [a] -> IO [b]
mapMConcurrent mapFn [] = return []
mapMConcurrent mapFn [a] =
   do
      b <- mapFn a
      return [b]
mapMConcurrent mapFn as =
   do
      (mVars :: [MVar b]) <- mapM
         (\ a ->
            do
               mVar <- newEmptyMVar
               let
                  act =
                     do
                        b <- mapFn a
                        putMVar mVar b
               forkIO act
               return mVar
            )
         as
      mapM takeMVar mVars

-- this version is careful to propagate exceptions, at a slight cost.
mapMConcurrentExcep :: forall a b . (a -> IO b) -> [a] -> IO [b]
mapMConcurrentExcep mapFn [] = return []
mapMConcurrentExcep mapFn [a] =
   do
      b <- mapFn a
      return [b]
mapMConcurrentExcep mapFn as =
   do
      (mVars :: [MVar (Either SomeException b)]) <- mapM
         (\ a ->
            do
               mVar <- newEmptyMVar
               let
                  act =
                     do
                        bAnswer <- Control.Exception.try (mapFn a)
                        putMVar mVar bAnswer
               forkIO act
               return mVar
            )
         as
      mapM
         (\ mVar ->
            do
               bAnswer <- takeMVar mVar
               propagate bAnswer
            )
         mVars



mapMConcurrent_ :: (a -> IO ()) -> [a] -> IO ()
mapMConcurrent_ mapFn as = mapM_ (\ a -> forkIO (mapFn a)) as

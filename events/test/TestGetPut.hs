{- Test that runs GetPut.hs.  Enter the number of times to test each type. -}
module Main(main) where

import Concurrent

import GetPut
import IOExts
import Channels
import NullGuard
import EqGuard
import RegexChannel

main :: IO ()
main =
   do
      lineIn <- getLine
      let num = read lineIn
      
      (mVar :: (MVar (IORef ()))) <- newEmptyMVar
      runTest num mVar

      (mVar :: (MVar (MVar ()))) <- newEmptyMVar
      runTest num mVar

      (mVar :: (MVar (Channel ()))) <- newEmptyMVar
      runTest num mVar

      (mVar :: (MVar (NullGuardedChannel ()))) <- newEmptyMVar
      runTest num mVar

      (mVar :: (MVar (EqGuardedChannel Int ()))) <- newEmptyMVar
      runTest num mVar

      (mVar :: (MVar RegexChannel)) <- newEmptyMVar
      runTest num mVar

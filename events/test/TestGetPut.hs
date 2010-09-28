{- Test that runs GetPut.hs.  Enter the number of times to test each type. -}
module Main(main) where

import Data.IORef

import CPUTime

import Control.Concurrent.MVar

import Util.Computation

import Events.Events
import Events.GuardedEvents
import Events.Channels
import Events.NullGuard
import Events.EqGuard
import Events.RegexChannel

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

class GetPut cell where
   name :: cell -> String
   new :: IO cell
   put :: cell -> IO ()
   get :: cell -> IO ()

instance GetPut (IORef ()) where
   name _ = "IORef"
   new = newIORef ()
   put ioR = writeIORef ioR ()
   get ioR = readIORef ioR

instance GetPut (MVar ()) where
   name _ = "MVar"
   new = newEmptyMVar
   put mVar = putMVar mVar ()
   get mVar = takeMVar mVar

instance GetPut (Channel ()) where
   name _ = "Channel"
   new = newChannel
   put chan = sync (noWait(send chan ()))
   get chan = sync (receive chan)

instance GetPut (NullGuardedChannel ()) where
   name _ = "NullGuard"
   new = newNullGuardedChannel
   put chan = sync(noWait(send chan ()))
   get chan = sync(receive chan)

instance GetPut (EqGuardedChannel Int ()) where
   name _ = "EqGuard"
   new = newEqGuardedChannel
   put chan = sync(noWait(send chan (1,())))
   get chan =
      do
         sync(toEvent(listen chan |> Eq (1::Int)))
         done

instance GetPut RegexChannel where
   name _ = "Regex"
   new = newRegexChannel
   put chan = sync(noWait(sendString chan "foo"))
   get chan =
      do
         sync (matchAny chan)
         done

runTest :: GetPut chan => Int -> MVar chan -> IO ()
runTest nTimes mVar =
   do
      chan <- new
      putMVar mVar chan
      start <- getCPUTime
      sequence_ [ (put chan) >> (get chan) | i <- [1..nTimes] ]
      end <- getCPUTime
      let
         (average :: Double) = (fromIntegral (end - start))/
            (1.0e6*fromIntegral nTimes)
      putStrLn ((name chan)++":nTimes="++(show nTimes)++" av="++
         show average++" microseconds")

{-# SPECIALIZE runTest :: Int -> MVar(IORef ()) -> IO () #-}
{-# SPECIALIZE runTest :: Int -> MVar(MVar ()) -> IO () #-}
{-# SPECIALIZE runTest :: Int -> MVar(NullGuardedChannel ()) -> IO () #-}
{-# SPECIALIZE runTest :: Int -> MVar(EqGuardedChannel Int ()) -> IO () #-}

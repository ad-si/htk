-- | A toggle is a switch initially True, which can only be made false
-- (when some action is performed, say).  This module implements
-- toggles, allowing atomic switching to false of 1 toggle, or
-- 2 toggles together.  To avoid deadlock we use a supply of unique
-- integers.
module Events.Toggle(
   Toggle, -- toggle type
   newToggle, -- create a new toggle
   toggle1, -- set the toggle to false, and return the original value.
   toggle2, -- if toggles are both true, change them to false, otherwise
   -- leave the toggle settings unchanged and return them.
   ifToggle, -- :: Toggle -> IO () -> IO ()
   -- If the toggle is true, change it to false and execute action.
   peekToggle, -- :: Toggle -> IO Bool
   -- peek at the contents of a toggle, without changing it.

   SimpleToggle, -- A simple toggle.  We can only flip these one at a time.
   newSimpleToggle, -- create a new simple toggle
   simpleToggle, -- set this toggle to false, and return the original value.
   ifSimpleToggle, -- like ifToggle
   ) where

import Control.Concurrent

import Util.Computation
import Util.Object

-- ----------------------------------------------------------------------
-- Simple Toggles
-- ----------------------------------------------------------------------

newtype SimpleToggle = SimpleToggle (MVar Bool)

newSimpleToggle :: IO SimpleToggle
newSimpleToggle =
   do
      mVar <- newMVar True
      return (SimpleToggle mVar)

simpleToggle :: SimpleToggle -> IO Bool
simpleToggle (SimpleToggle mVar) =
   do
      oldVal <- takeMVar mVar
      putMVar mVar False
      return oldVal

ifSimpleToggle :: SimpleToggle -> IO () -> IO ()
ifSimpleToggle sToggle action =
   do
      goAhead <- simpleToggle sToggle
      if goAhead then action else done

-- simpleToggle2 is not safe from deadlocks
simpleToggle2 :: SimpleToggle -> SimpleToggle -> IO (Maybe (Bool,Bool))
simpleToggle2 (SimpleToggle mVar1) (SimpleToggle mVar2) =
   do
      oldVal1 <- takeMVar mVar1
      oldVal2 <- takeMVar mVar2
      if (oldVal1 && oldVal2)
         then
            do
               putMVar mVar2 False
               putMVar mVar1 False
               return Nothing
         else
            do
               putMVar mVar2 oldVal2
               putMVar mVar1 oldVal1
               return (Just (oldVal1,oldVal2))


-- peekSimpleToggle is used by toggle2
peekSimpleToggle :: SimpleToggle -> IO Bool
peekSimpleToggle (SimpleToggle mVar) = readMVar mVar

-- ----------------------------------------------------------------------
-- Toggles
-- ----------------------------------------------------------------------

data Toggle = Toggle !ObjectID !SimpleToggle

newToggle :: IO Toggle
newToggle =
   do
      uniqVal <- newObject
      stoggle <- newSimpleToggle
      return (Toggle uniqVal stoggle)

toggle1 :: Toggle -> IO Bool
-- switch bool to false, returning original value.
toggle1 (Toggle _ stoggle) = simpleToggle stoggle

ifToggle :: Toggle -> IO () -> IO ()
ifToggle toggle action =
   do
      goAhead <- toggle1 toggle
      if goAhead then action else done

toggle2 :: Toggle -> Toggle -> IO(Maybe(Bool,Bool))
-- Switch both toggles to from True to False, atomically, if possible.
-- If we can't do this, return Just (the current status of the toggles).
toggle2 (Toggle unique1 stoggle1) (Toggle unique2 stoggle2) =
   case compare unique1 unique2 of
      LT -> simpleToggle2 stoggle1 stoggle2
      GT ->
         do
            result <- simpleToggle2 stoggle2 stoggle1
            case result of
               Nothing -> return Nothing
               Just (r1,r2) -> return (Just (r2,r1))
      EQ ->
         do
            r <- peekSimpleToggle stoggle1
            return (Just (r,r))

-- peekToggle is used in Channels.hs to avoid a memory leak.
peekToggle :: Toggle -> IO Bool
peekToggle (Toggle _ sToggle) = peekSimpleToggle sToggle


-- ----------------------------------------------------------------------
-- Optimisations
-- ----------------------------------------------------------------------


{-# INLINE newToggle #-}
{-# INLINE toggle1 #-}
{-# INLINE toggle2 #-}
{-# INLINE peekToggle #-}
{-# INLINE newSimpleToggle #-}
{-# INLINE simpleToggle #-}
{-# INLINE simpleToggle2 #-}





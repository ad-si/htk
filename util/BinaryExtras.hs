-- | This module contains various extra Binary instances, for example ones
-- which are particular GHC or uni-specific.
module BinaryExtras(
   hReadLtd, -- :: HasBinary a IO => Int -> Handle -> IO (WithError a)


   initialClockTime, -- :: ClockTime
      -- static clock time, used in other modules.
   ) where

import IO

import Data.IORef
import System.Time

import Binary
import BinaryUtils
import BinaryInstances

import Computation
import ExtendedPrelude
import IOExtras

-- | Read something, but throw an exception if there is an attempt to
-- read more than the number of characters given by the first argument. 
hReadLtd :: HasBinary a IO => Int -> Handle -> IO (WithError a)
hReadLtd limit handle =
   addFallOutWE (\ break ->
      do
         lenIORef <- newIORef 0
         let
            ensure :: Int -> IO ()
            ensure i =
               do
                  len1 <- simpleModifyIORef lenIORef
                     (\ len0 -> 
                        let
                           len1 = len0 + i
                        in
                           (len1,len1)
                        )
                  if len1 > limit
                     then
                        break "BinaryExtras.hReadLtd: limit exceeded"
                     else
                        done


            (ReadBinary {readByte = readByte1,readBytes = readBytes1}) 
               = toReadBinaryHandle handle

            readByte2 =
               do
                  ensure 1
                  readByte1
            readBytes2 len =
               do
                  ensure len
                  readBytes1 len

            rb2 = ReadBinary {readByte = readByte2,readBytes = readBytes2}

         readBin rb2
      )
   
-- ----------------------------------------------------------------------
-- Instance for ClockTime
-- ----------------------------------------------------------------------


instance Monad m => HasBinary ClockTime m where
   writeBin = mapWrite (\ (TOD i j) -> (i,j))
   readBin = mapRead (\ (i,j) -> TOD i j)

-- | Time this code was written.  We bung this definition in here
-- because this module needs GHC-specific access to ClockTime anyway.
initialClockTime :: ClockTime
initialClockTime = TOD 1052391874 190946000000


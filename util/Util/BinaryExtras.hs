{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module contains various extra Binary instances, for example ones
-- which are particular GHC or uni-specific.
module Util.BinaryExtras(
   hReadLtd, -- :: HasBinary a IO => Int -> Handle -> IO (WithError a)


   initialClockTime, -- :: ClockTime
      -- static clock time, used in other modules.
   ) where

import System.IO

import Data.IORef
import System.Time

import Util.Binary
import Util.BinaryUtils

import Util.Computation
import Util.ExtendedPrelude
import Util.IOExtras
import Util.BinaryInstances()

-- | Read something, but throw an exception if there is an attempt to
-- read too many characters.
hReadLtd :: HasBinary a IO =>
   Int -- ^ the maximum number of characters
   -> Handle -> IO (WithError a)
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


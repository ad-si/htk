{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module interfaces the strerror function.
module Util.StrError(
   strError, -- :: Int -> IO String
   ) where

import System.IO.Unsafe
import Foreign.C.String

import Control.Concurrent.MVar

strError :: Int -> IO String
strError i =
   modifyMVar lock (\ () ->
      do
         cString <- strErrorPrim i
         str <- peekCString cString
         return ((),str)
      )


foreign import ccall unsafe "string.h strerror" strErrorPrim
   :: Int -> IO CString

lock :: MVar ()
lock = unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

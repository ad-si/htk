#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC
{-# OPTIONS -#include "callWish.h" #-}
#endif /* NEW_GHC */

{- This is the bare-bones interface which actually calls the wish program.
   This is the Windows version.

   NB.  The Windows interface relies on callWish only ever being used once in
   a program, because it relies on storing crucial information in static data.
   -}
module CallWish(
   CalledWish, -- Type representing wish instance
   callWish, -- :: IO CalledWish
   sendCalledWish, -- :: CalledWish -> CStringLen -> IO ()
      -- sendCalledWish is given the data in ByteArray/Length form.
   readCalledWish, -- :: CalledWish -> IO String
   destroyCalledWish, -- :: CalledWish -> IO ()
   ) where

import MarshalAlloc
import MarshalArray
import ByteArray
import CString
import CTypesISO
import CTypes
import Ptr
import Storable

import Concurrent

import WBFiles

-- -----------------------------------------------------------------------
-- Foreign function interface to the three C functions
-- -----------------------------------------------------------------------


#ifndef NEW_GHC
foreign import "initialise_wish" unsafe initialiseWishPrim :: CString -> IO () 
#else /* NEW_GHC */
foreign import ccall unsafe "callWish.h initialise_wish" initialiseWishPrim :: CString -> IO ()
#endif /* NEW_GHC */

initialiseWish :: String -> IO ()
initialiseWish wishPath =
  do
     cString <- newCString wishPath
     initialiseWishPrim cString

#ifndef NEW_GHC
foreign import "write_to_wish" unsafe writeToWishPrim :: CString -> CSize -> IO ()
#else /* NEW_GHC */
foreign import ccall unsafe "callWish.h write_to_wish" writeToWishPrim :: CString -> CSize -> IO ()
#endif /* NEW_GHC */

#ifndef NEW_GHC
foreign import "get_readwish_fd" unsafe getReadWishFdPrim :: IO CInt
#else
foreign import ccall unsafe "callwish.h get_readwish_fd" getReadWishFdPrim 
   :: IO CInt
#endif

getReadWishFd :: IO Int
getReadWishFd =
   do
     cfd <- getReadWishFdPrim
     return (fromIntegral cfd)

#ifndef NEW_GHC
foreign import "read_from_wish" unsafe readFromWishPrim 
   :: CString -> CSize -> IO CSize
#else /* NEW_GHC */
foreign import ccall unsafe "callWish.h read_from_wish" readFromWishPrim 
   :: CString -> CSize -> IO CSize
#endif /* NEW_GHC */

#ifndef NEW_GHC
foreign import "read_from_wish_avail" unsafe readFromWishAvail :: IO CSize
#else
foreign import ccall unsafe "callWish.h readFromWishAvail" readFromWishAvail :: 
IO CSize
#endif

readFromWish :: IO String
#ifdef GHC_CONCURRENCY_FIXED
{-  This is how we SHOULD do it.  Unfortunately, GHC on Windows provides
    no mechanism for waiting on a file which doesn't stop the world.
    -}
readFromWish =
   do
      -- Make sure there is input available, but try to avoid blocking.
      readWishFd <- getReadWishFd
      threadWaitRead readWishFd

      let bufferSize = 100 
         -- absurdly large for most answers from Wish we will need, in fact

      -- We allocate the buffer using allocArray, which hopefully is more
      -- efficient than mallocArray/free.
      allocaArray bufferSize
         (\ (buffer :: Ptr CChar) ->
            do
               resultSize 
                  <- readFromWishPrim buffer (fromIntegral bufferSize) 
               peekCStringLen (buffer,fromIntegral resultSize)
            )

#else
{- So what we actually do, sadly, is check every so often (currently
   every tenth of a second) to see if input is available. -}

readFromWish =
   do
      let 
         bufferSize = 100 
         -- absurdly large for most answers from Wish we will need, in fact

         waitTick = Concurrent.threadDelay 100000

         readToBuffer (buffer :: Ptr CChar) =
            do
               bytesAvail <- readFromWishAvail
               if bytesAvail > 0
                  then
                     do
                        resultSize 
                           <- readFromWishPrim buffer (fromIntegral bufferSize) 
                        peekCStringLen (buffer,fromIntegral resultSize)
                  else
                     do
                        waitTick
                        readToBuffer buffer

      -- We allocate the buffer using allocArray, which hopefully is more
      -- efficient than mallocArray/free.
      allocaArray bufferSize readToBuffer

#endif


-- -----------------------------------------------------------------------
-- Implementation of CallWish functions.  The main excitement is that
-- we need to replace readWish by something which gets the next line,
-- requiring us to keep a buffer of already-read characters.
-- -----------------------------------------------------------------------

newtype CalledWish = CalledWish (MVar String)

callWish :: IO CalledWish
callWish = 
   do
      wishPath <- getWishPath
      initialiseWish wishPath
      mVar <- newMVar ""
      return (CalledWish mVar)

sendCalledWish :: CalledWish -> CStringLen -> IO ()
sendCalledWish _ (cString,len) =
   writeToWishPrim cString (fromIntegral len)

-- I don't know how you destroy a child process in Windows.  Hopefully
-- asking it nicely will work.
destroyCalledWish :: CalledWish -> IO ()
destroyCalledWish calledWish = 
   do
      cStringLen <- newCStringLen "exit\n"
      sendCalledWish calledWish cStringLen

-- This function is copied (with minor modifications) from ChildProcess.readMsg.
readCalledWish :: CalledWish -> IO String
readCalledWish (CalledWish mVar) = 
   do
      buffer <- takeMVar mVar
      (newBuffer,result) <- readWithBuffer buffer []
      putMVar mVar newBuffer
      return result
   where
      readWithBuffer [] acc = 
      -- we use an accumulating parameter since I don't want a
      -- non-tail-recursive action.
         do
            nextChunk <- readFromWish
            readWithBuffer nextChunk acc
      readWithBuffer ('\n' : rest) acc =
         return (rest,reverse (trimReturn acc))
      readWithBuffer (other : rest) acc =
         readWithBuffer rest (other : acc)

      -- the joy of Windows . . .
      trimReturn ('\r':line) = line




   

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This defines primitive byte operations, to be used with binary conversion.
-- For the present we use the FFI.  There are probably lots of better ways.

module Util.Bytes(
   Byte,
      -- this type is expected to be an instance of Eq, Ord, Num, Bits,
      -- Integral, Show and contain (at least) the values 0..255.
   Bytes,
      -- an array of values of type Byte.
      -- NB.  The caller is responsible for making sure writes to and from
      -- this array are within bounds.


   putByteToBytes,
      -- :: Byte -> Bytes -> Int -> IO ()
      -- write byte to index.
   getByteFromBytes,
      -- :: Bytes -> Int -> IO Byte

   putBytesToBytes,
      -- :: Bytes -> Int -> Byte -> Int -> Int -> IO ()
      -- putBytesToBytes source sourceIndex dest destIndex length
      --    copies length bytes starting at source[sourceIndex] to
      --    dest[destIndex]
      -- It assumes that the source and destination areas don't overlap.
   hPutByte,
      -- :: Handle -> Byte -> IO ()
   hGetByte,
      -- :: Handle -> IO Byte

   hPutBytes,
      -- :: Handle -> Bytes -> Int -> IO ()
   hGetBytes,
      -- :: Handle -> Int -> IO Bytes
      -- hGetBytes allocates an area, which needs to be
      -- freed using freeBytes.

   -- the following are similar to C's malloc/alloc/realloc/free.
   bytesMalloc,
      -- :: Int -> IO Bytes
   bytesReAlloc,
      -- :: Bytes -> Int -> IO Bytes.
   bytesAlloca,
      -- :: Int -> (Bytes -> IO a) -> IO a
   bytesFree,
      -- :: Bytes -> IO ()

   withBytesAsCChars,
      -- :: Bytes -> (Ptr CChar -> IO a) -> IO a
      -- This gives you access to the contents of Bytes as a Ptr CChar.
      -- The length will be the number of Bytes in the array.
      -- NB.  The Ptr CChar may become invalid (or garbage) after the
      -- function supplied by the caller returns.

   mkBytes,
      -- :: Ptr CChar -> Bytes
   unMkBytes,
      -- :: Bytes -> Ptr CChar
      -- low-level interface (and therefore likely to change)


   compareBytes, -- :: Bytes -> Bytes -> Int -> IO Ordering
      -- Compare two Bytes items up to the given length, in a consistent
      -- way.
   ) where

-- FFI imports
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- Other GHC imports.
import Data.Bits(Bits)
import Data.Char

import System.IO

import System.IO.Error
import Control.Exception (throw)

-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------

newtype Byte = Byte CUChar deriving (Eq,Ord,Num,Bits,Show,Real,Enum,Integral)

newtype Bytes = Bytes (Ptr CChar)

-- ----------------------------------------------------------------------
-- The exported functions
--  ----------------------------------------------------------------------

putByteToBytes :: Byte -> Bytes -> Int -> IO ()
putByteToBytes (Byte u) (Bytes ptr) i
   = pokeArray (advancePtr ptr i) [fromIntegral u]

getByteFromBytes :: Bytes -> Int -> IO Byte
getByteFromBytes (Bytes ptr) i =
   do
      [c] <- peekArray 1 (advancePtr ptr i)
      return (Byte (fromIntegral c))

putBytesToBytes :: Bytes -> Int -> Bytes -> Int -> Int -> IO ()
putBytesToBytes (Bytes sourcePtr) sourceIndex (Bytes destPtr) destIndex len
   = copyArray (advancePtr destPtr destIndex)
      (advancePtr sourcePtr sourceIndex) len

hPutByte :: Handle -> Byte -> IO ()
hPutByte handle (Byte u) = hPutChar handle (chr (fromIntegral u))

hGetByte :: Handle -> IO Byte
hGetByte handle =
   do
      char <- hGetChar handle
      return (Byte (fromIntegral (ord char)))

hPutBytes :: Handle -> Bytes -> Int -> IO ()
hPutBytes handle (Bytes ptr) len =
   hPutBuf handle ptr len

hGetBytes ::  Handle -> Int -> IO Bytes
hGetBytes handle len =
   do
      (bytes@(Bytes ptr)) <- bytesMalloc len
      lenRead <- hGetBuf handle ptr len
      if lenRead < len
         then
            do
               bytesFree bytes
               throwEOF handle
         else
            return bytes

bytesMalloc :: Int -> IO Bytes
bytesMalloc i =
   do
      ptr <- mallocBytes i
      return (Bytes ptr)

bytesReAlloc :: Bytes -> Int -> IO Bytes
bytesReAlloc (Bytes ptr1) newLen =
   do
      ptr2 <- reallocBytes ptr1 newLen
      return (Bytes ptr2)

bytesAlloca :: Int -> (Bytes -> IO a) -> IO a
bytesAlloca len fn = allocaBytes len (\ ptr -> fn (Bytes ptr))

bytesFree :: Bytes -> IO ()
bytesFree (Bytes ptr) = free ptr


withBytesAsCChars :: Bytes -> (Ptr CChar -> IO a) -> IO a
withBytesAsCChars (Bytes ptr) fn = fn ptr



mkBytes :: Ptr CChar -> Bytes
mkBytes = Bytes

unMkBytes :: Bytes -> Ptr CChar
unMkBytes (Bytes ptr) = ptr

-- ----------------------------------------------------------------------
-- Throw an EOF error
-- ----------------------------------------------------------------------

throwEOF :: Handle -> IO a
throwEOF handle =
   do
      let
         eofError =
            mkIOError eofErrorType
               "BinaryIO" (Just handle)
               Nothing
      throw eofError

-- ----------------------------------------------------------------------
-- Compare two Bytes values in an unspecified but consistent way.
-- ----------------------------------------------------------------------

compareBytes :: Bytes -> Bytes -> Int -> IO Ordering
compareBytes (Bytes p1) (Bytes p2) len =
   do
      res <- compareBytesPrim p1 p2 (fromIntegral len)
      return (compare res 0)

foreign import ccall unsafe "string.h memcmp"
   compareBytesPrim :: Ptr CChar -> Ptr CChar -> CSize -> IO CInt

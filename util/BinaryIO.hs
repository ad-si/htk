{- This module contains various code for writing and reading things to and
   from a Handle efficiently.  In particular CStringLens.  -}
module BinaryIO(
   HasBinaryIO(..), -- class of things that can be encoded.
      -- instances defined: 
      --    CStringLen, 
      --    ReadShow a where a instances Read and Show
   ReadShow(..),

   HasConverter(..), -- class for converting to and from binary representations
   CodedList(..), -- binary representation (basically a list of integers)
   topBit, -- top bit of characters, as an integer.
   chrGeneral, -- functions for converting integers to and from Char.
   ordGeneral,
   ) where

import IO
import Char

import Data.Bits
import Data.PackedString
import GHC.Int(Int32)
import Foreign.C.String
import GHC.IO
import Foreign.Marshal.Alloc

import Computation

-- ---------------------------------------------------------------------------
-- The HasBinaryIO class
-- ---------------------------------------------------------------------------

class HasBinaryIO value where
   hPut :: Handle -> value -> IO ()
   hGet :: Handle -> IO value
   hGetWE :: Handle -> IO (WithError value)
      -- this catches parsing errors (but not IO errors).

   hGetInt :: Int -> Handle -> IO value
   hGetIntWE :: Int -> Handle -> IO (WithError value)
      -- this allows an extra integer argument to be passed to the parsing
      -- function, which is useful in some cases. 

      -- minimally either hGetWE or hGetIntWE should be defined.

   hGet handle = 
      do
         value <- hGetWE handle
         coerceWithErrorIO value

   hGetInt i handle = 
      do
         value <- hGetIntWE i handle
         coerceWithErrorIO value

   hGetWE = hGetIntWE 0

   hGetIntWE _ = hGetWE

-- ---------------------------------------------------------------------------
-- Instances of Read/Show
-- ---------------------------------------------------------------------------

newtype ReadShow a = ReadShow a

instance (Read a,Show a) => HasBinaryIO (ReadShow a) where
   hPut handle (ReadShow value) = hPutStrLn handle (show value)
   hGetWE handle =
      do
         str <- hGetLine handle
         case readsPrec 0 str of
            [(value,"")] -> return (hasValue (ReadShow value))
            [(value,extra)] -> return (hasError (
               "Extra characters parsing " ++ show str))
            _ -> return (hasError ("Couldn't parse " ++ show str))

-- ---------------------------------------------------------------------------
-- String
-- ---------------------------------------------------------------------------

instance HasBinaryIO String where
   hPut handle str = hPut handle (packString str)
   hGetIntWE limit handle =
      do
         packedWE <- hGetIntWE limit handle
         return (mapWithError unpackPS packedWE)

-- ---------------------------------------------------------------------------
-- PackedString
-- ---------------------------------------------------------------------------

instance HasBinaryIO PackedString where
   hPut handle packedString =
      do
         hPut handle (lengthPS packedString :: Int)
         hPutPS handle packedString
   hGetIntWE limit handle =
      do
         (lenWE :: WithError Int) <- hGetIntWE limit handle
         mapWithErrorIO'
            (\ len ->
               do
                  packed <- hGetPS handle len
                  return (hasValue packed)
               )
            lenWE

-- ---------------------------------------------------------------------------
-- CStringLen
-- ---------------------------------------------------------------------------

instance HasBinaryIO CStringLen where
   -- NB NB.  It is the responsibility of the caller of the hGet* functions
   -- to deallocate the CString.  This may be done using 
   -- Foreign.Marshal.Alloc.Free.

   hPut handle (cString,len) =
      do
         hPut handle len
         hPutBuf handle cString len
   hGetIntWE i handle =
      do
         (lenWE :: WithError Int) <- hGetIntWE i handle
         mapWithErrorIO'
            (\ len ->
               do
                  -- need to allocate space first.
                  cStringPtr <- mallocBytes len 
                  lenRead <- hGetBuf handle cStringPtr len
                  if lenRead < len
                     then
                        do
                           free cStringPtr
                           return (hasError ("EOF within BinaryIO"))
                     else
                        return (hasValue (cStringPtr,len))
               )
            lenWE

   hGetWE = hGetIntWE 4 -- allow a higher default
-- ---------------------------------------------------------------------------
-- CodedList's.  These are used indirectly for integers.
-- ---------------------------------------------------------------------------

newtype CodedList = CodedList [Int32] 
-- This is a nonempty list of integers in [0,2^(bitsInChar-1)).

instance HasBinaryIO CodedList where
   -- we set the top bit of the last list item.
   hPut handle (CodedList [i]) =
      hPutChar handle (chrGeneral (i .|. topBit))
   hPut handle (CodedList (i:is)) =
      do
         hPutChar handle (chrGeneral i)
         hPut handle (CodedList is)

   -- the integer is a bound on the maximum length
   hGetIntWE limit handle =
      if limit <= 0
         then
            return (hasError "Overflow trying to read integer value")
         else
            do
               ch <- hGetChar handle
               let
                  i = ordGeneral ch
               if i >= topBit
                  then
                     return (hasValue (CodedList [i `xor` topBit]))
                  else
                     do
                        clWE <- hGetIntWE (limit-1) handle
                        return (mapWithError 
                           (\ (CodedList is) -> 
                              CodedList (i : is)
                              )
                           clWE
                           )

chrGeneral :: Integral a => a -> Char
chrGeneral value = chr (fromIntegral value)

ordGeneral :: Integral a => Char -> a
ordGeneral value = fromIntegral (ord value)

-- ---------------------------------------------------------------------------
-- Integers
-- ---------------------------------------------------------------------------

instance (Integral integral,Bits integral) => HasBinaryIO integral where
   hPut handle int = hPut handle (encode' int :: CodedList)
   hGetIntWE i handle =
      do
         (clWE :: WithError CodedList) <- hGetIntWE i handle
         return (mapWithError decode' clWE)
   hGetWE = hGetIntWE 4

bitsInChar :: Int
-- Number of bits easily stored in the Char type.  Thus if Unicode ends
-- up getting stored as UTF8 we may prefer to change this to 7 or 16.
bitsInChar = 8

bitsPerChar :: Int
-- Number of bits of an integer we will store per char.
-- (The remaining one is used to mark the end of the sequence.)
bitsPerChar = bitsInChar - 1

-- Here are some useful abbreviations in this connection
topBit :: Bits integral => integral
topBit = bit bitsPerChar

mask :: (Integral integral,Bits integral) => integral
mask = topBit - 1

nextBit :: Bits integral => integral
nextBit = bit (bitsInChar - 2)

class HasConverter value1 value2 where
   encode' :: value1 -> value2
   decode' :: value2 -> value1

instance (Integral integral,Bits integral) 
   => HasConverter integral CodedList 
      where
   encode' i =
      if (i >= nextBit) || (i < -nextBit) 
         then
            let
               lowestPart = i .&. mask
               highPart = i `shiftR` bitsPerChar
               CodedList codedHigh = encode' highPart 
            in
               CodedList ((fromIntegral lowestPart) : codedHigh)
         else
            let
               wrapped =
                  if i < 0
                     then
                        topBit + i
                     else
                        i
            in
               CodedList [fromIntegral wrapped]
   decode' (CodedList [wpped]) =
      let
         wrapped = fromIntegral wpped
      in
         if wrapped >= nextBit
            then
               wrapped - topBit
            else
               wrapped
   decode' (CodedList (lPart : codedHigh)) =
      let
         lowestPart = fromIntegral lPart
         highPart = decode' (CodedList codedHigh) 
      in
         lowestPart + (highPart `shiftL` bitsPerChar)



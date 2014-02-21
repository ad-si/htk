{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Library for converting types to and from binary, so that they can
be written to and from files, stored compactly in memory, and so on.

This is a preliminary version of the library, hence I have decided
/not/ to optimise heavily, beyond putting in strictness annotations
in where they seem appropriate.

A good place to start optimising would probably be the separate
"Bytes" libary.

See also "BinaryInstances", which declares instances for the standard
types (and one or two others), "BinaryUtils", which contains
(mostly) material for declaring new instances, "BinaryExtras",
which contains other miscellaneous utilities, and finally
"BinaryAll" which just imports and reexports everything. -}
module Util.Binary (

   hWrite, -- :: HasBinary a IO => Handle -> a -> IO ()
   hRead, -- :: HasBinary a IO => Handle -> IO a

   writeToBytes, -- :: HasBinary a StateBinArea => a -> IO (Bytes,Int)
   writeToBytes0, -- :: HasBinary a StateBinArea => Int -> a -> IO (Bytes,Int)
   readFromBytes, -- :: HasBinary a StateBinArea => (Bytes,Int) -> IO a


   HasBinary (..),
   WriteBinary (..),
   ReadBinary (..),

   {- Ways of constructing WriteBinary/ReadBinary instances (not usually
   required explicitly). -}
   toWriteBinaryHandle, -- :: Handle -> WriteBinary IO
   toReadBinaryHandle, -- :: Handle -> ReadBinary IO

   -- Functions required for writing directly to binary areas.
   BinArea,
   StateBinArea, -- = StateT BinArea IO

   -- writing a BinArea

   -- create
   mkEmptyBinArea, {- :: Int -> IO BinArea
   pass as argument to writeBin -}
   writeBinaryBinArea, {- :: WriteBinary StateBinArea
   close and get contents. -}
   closeBinArea, -- :: BinArea -> IO (Bytes,Int)

   -- reading a BinArea

   -- create
   mkBinArea, {- :: (Bytes,Int) -> BinArea
   pass to things which read. -}
   readBinaryBinArea, {- :: ReadBinary StateBinArea
   check that the BinArea is completely read. -}
   checkFullBinArea, -- :: BinArea -> IO ()


   -- Functions for transforming WriteBinary/ReadBinary values.
   liftWriteBinary,
      -- :: (forall a . m a -> n a) -> WriteBinary m -> WriteBinary n
   liftReadBinary,
      -- :: (forall a . m a -> n a) -> ReadBinary m -> ReadBinary n

   ) where

-- Standard imports
import System.IO

-- GHC imports
import Control.Monad.State

-- Our imports
import Util.Bytes

{- ----------------------------------------------------------------------
The general framework
Type variable "m" is a monad; "a" is the thing to read or write.

NB.  Bytes values are currently not subject to the garbage-collector,
and so need to be explicitly freed.   The following rules for this
should be observed.

(1) For writeBytes, it is only guaranteed that the argument "Bytes"
will be valid at the actual time of evaluation.
(2) For readBytes, it is the caller's responsibility to free the returned
area.
---------------------------------------------------------------------- -}

-- | A consumer of binary data
data WriteBinary m =
   WriteBinary {
      writeByte :: Byte -> m (),
         -- ^ write one byte
      writeBytes :: Bytes -> Int -> m ()
         -- ^ write multiple bytes
      }

-- | A source of binary data
data ReadBinary m =
   ReadBinary {
      readByte :: m Byte,
         -- ^ read one byte
      readBytes :: Int -> m Bytes
         -- ^ read multiple bytes
      }

class HasBinary a m where
   writeBin :: WriteBinary m -> a -> m ()
      -- ^ Given a consumer of binary data, and an (a), write out the (a)
   readBin :: ReadBinary m -> m a
      -- ^ Given a source of binary data, provide an (a)

{- ----------------------------------------------------------------------
Reading/Writing HasBinary instances to Handles.
---------------------------------------------------------------------- -}

-- | Write an (a) to a 'Handle'
hWrite :: HasBinary a IO => Handle -> a -> IO ()
hWrite handle = writeBin $ toWriteBinaryHandle handle


-- | Read an (a) from a 'Handle'
hRead :: HasBinary a IO => Handle -> IO a
hRead handle = readBin (toReadBinaryHandle handle)

toWriteBinaryHandle :: Handle -> WriteBinary IO
toWriteBinaryHandle handle =
   WriteBinary {
      writeByte = hPutByte handle,
      writeBytes = hPutBytes handle
      }

toReadBinaryHandle :: Handle -> ReadBinary IO
toReadBinaryHandle handle =
   ReadBinary {
      readByte = hGetByte handle,
      readBytes = hGetBytes handle
      }

{- ----------------------------------------------------------------------
Writing HasBinary instances to a memory area

We do this by allocating an area, and then doubling its size as
necessary.
---------------------------------------------------------------------- -}

-- | Somewhere to where you write binary data in memory.
data BinArea = BinArea {
   bytes :: ! Bytes, -- current storage area
   len :: ! Int, -- its length
   next :: ! Int -- where to write next bit of data.
   }

-- | Write an (a) to memory.  The 'Int' is the length of the area.
writeToBytes :: HasBinary a StateBinArea => a -> IO (Bytes, Int)
writeToBytes = writeToBytes0 1000
   {- Be generous, since memory is cheap.  Make it a bit less than a power
   of two, since some memory allocation algorithms (buddy algorithm)
   like this. -}

{- | Write an (a) to memory.
The integer argument is an initial guess at the number of bytes
that will be needed.  This should be greater than 0.  If it is
too small, there will be unnecessary reallocations; if too large,
too much memory will be used. -}
writeToBytes0 :: HasBinary a StateBinArea => Int -> a -> IO (Bytes, Int)
-- The result is returned as a pair (data area,length)
writeToBytes0 len0 a =
   do
      binArea0 <- mkEmptyBinArea len0
      ((), binArea1) <- runStateT (writeBin writeBinaryBinArea a) binArea0
      closeBinArea binArea1

-- | Create an empty 'BinArea', given the initial size.
mkEmptyBinArea :: Int -> IO BinArea
-- the argument gives the initial size to use (which had better be positive).
mkEmptyBinArea l =
   do
      bs <- bytesMalloc l
      return BinArea {
         bytes = bs,
         len = l,
         next = 0
         }

-- | Return all the data currently in the 'BinArea'
closeBinArea :: BinArea -> IO (Bytes, Int)
closeBinArea binArea =
   do
      let
         bytes1 = bytes binArea
         l = next binArea
      bytes2 <- bytesReAlloc bytes1 l
      return (bytes2, l)

-- | a state monad containing the BinArea.
type StateBinArea = StateT BinArea IO

-- | A 'BinArea' as somewhere to put binary data.
writeBinaryBinArea :: WriteBinary StateBinArea
writeBinaryBinArea = WriteBinary {
   writeByte = \ byte ->
      StateT $ \ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + 1
            binArea1 <- ensureBinArea binArea0 next1
            putByteToBytes byte (bytes binArea1) next0
            return ((), binArea1 {next = next1})
   , writeBytes = \ bytes' l ->
      StateT $ \ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + l
            binArea1 <- ensureBinArea binArea0 next1
            putBytesToBytes bytes' 0 (bytes binArea1) next0 l
            return ((), binArea1 {next = next1})
   }

-- | ensure that the given BinArea can hold at least len bytes.
ensureBinArea :: BinArea -> Int -> IO BinArea
ensureBinArea binArea size =
   if size <= len binArea
      then
         return binArea
      else
        do
           let
              len1 = 2 * size
           bytes1 <- bytesReAlloc (bytes binArea) len1
           return BinArea {
              bytes = bytes1,
              len = len1,
              next = next binArea
              }

{- ----------------------------------------------------------------------
Reading Binary instances from a memory area
We use BinArea's for this too.  But this is simpler, because we don't have to
worry about reallocing.
---------------------------------------------------------------------- -}

{- | Read a value from binary data in memory.  The 'Int' is the length,
and there will be an error if this is either too small or too large. -}
readFromBytes :: HasBinary a StateBinArea => (Bytes, Int) -> IO a
readFromBytes bl =
   do
      let
         binArea0 = mkBinArea bl

      (a, binArea1) <- runStateT (readBin readBinaryBinArea) binArea0
      checkFullBinArea binArea1
      return a

{- | Turn binary data in memory into a 'BinArea' (so that you can
read from it). -}
mkBinArea :: (Bytes, Int) -> BinArea
mkBinArea (bytes', len') =
   BinArea {
      bytes = bytes',
      len = len',
      next = 0
      }

checkFullBinArea :: BinArea -> IO ()
checkFullBinArea binArea =
   unless (next binArea == len binArea) $
         error "Binary.checkFullBinArea: mysterious extra bytes"


-- | A BinArea as a source of binary data.
readBinaryBinArea :: ReadBinary StateBinArea
readBinaryBinArea = ReadBinary {
   readByte = StateT $ \ binArea0 ->
      do
         let
            next0 = next binArea0
            next1 = next0 + 1
         checkBinArea binArea0 next1
         byte <- getByteFromBytes (bytes binArea0) next0
         return (byte, binArea0 {next = next1})
   , readBytes = \ l ->
      StateT $ \ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + l
            checkBinArea binArea0 next1
            bytes' <- bytesMalloc l
            putBytesToBytes (bytes binArea0) next0 bytes' 0 l
            return (bytes', binArea0 {next = next1})
   }

checkBinArea :: BinArea -> Int -> IO ()
-- check that the given BinArea can hold at least len bytes.
checkBinArea binArea newNext =
   when (newNext > len binArea) $
         error "Binary.checkBinArea - BinArea overflow on read"

{- ----------------------------------------------------------------------
Lifting writeBinary and readBinary instances.
---------------------------------------------------------------------- -}

-- | Transform the monad used by a 'WriteBinary'
liftWriteBinary :: (forall a . m a -> n a) -> WriteBinary m -> WriteBinary n
liftWriteBinary lft wb =
   let
      writeByte2 b = lft (writeByte wb b)
      writeBytes2 b i = lft (writeBytes wb b i)
   in
      WriteBinary {writeByte = writeByte2, writeBytes = writeBytes2}

-- | Transform the monad used by a 'ReadBinary'
liftReadBinary :: (forall a . m a -> n a) -> ReadBinary m -> ReadBinary n
liftReadBinary lft rb =
   let
      readByte2 = lft (readByte rb)
      readBytes2 i = lft (readBytes rb i)
   in
      ReadBinary {readByte = readByte2, readBytes = readBytes2}

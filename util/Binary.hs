{- Library for converting types to and from binary, so that they can
   be written to and from files, stored compactly in memory, and so on.

   This is a preliminary version of the library, hence I have decided 
   *not* to optimise heavily, beyond putting in strictness annotations
   in where they seem appropriate.

   A good place to start optimising would probably be the separate
   Bytes libary.
   -}
module Binary (

   hWrite, -- :: HasBinary a IO => Handle -> a -> IO ()
   hRead, -- :: HasBinary a IO => Handle -> IO a

   writeToBytes, -- :: HasBinary a StateBinArea => a -> IO (Bytes,Int)
   writeToBytes0, -- :: HasBinary a StateBinArea => Int -> a -> IO (Bytes,Int)
   readFromBytes, -- :: HasBinary a StateBinArea => (Bytes,Int) -> IO a


   HasBinary(..),
   WriteBinary(..),
   ReadBinary(..),

   -- Ways of constructing WriteBinary/ReadBinary instances (not usually
   -- required explicitly).
   toWriteBinaryHandle, -- :: Handle -> WriteBinary IO
   toReadBinaryHandle, -- :: Handle -> ReadBinary IO

   -- Functions required for writing directly to binary areas.
   BinArea,
   StateBinArea, -- = StateT BinArea IO

   -- writing a BinArea

   -- create
   mkEmptyBinArea, -- :: Int -> IO BinArea
   -- pass as argument to writeBin
   writeBinaryBinArea, -- :: WriteBinary StateBinArea
   -- close and get contents.
   closeBinArea, -- :: BinArea -> IO (Bytes,Int)

   -- reading a BinArea

   -- create
   mkBinArea, -- :: (Bytes,Int) -> BinArea
   -- pass to things which read.
   readBinaryBinArea, -- :: ReadBinary StateBinArea
   -- check that the BinArea is completely read. 
   checkFullBinArea, -- :: BinArea -> IO ()
  
   ) where

-- Standard imports
import IO

-- GHC imports
import Control.Monad.State

-- Our imports
import Bytes

-- ----------------------------------------------------------------------
-- The general framework
-- Type variable "m" is a monad; "a" is the thing to read or write.
--
-- NB.  Bytes values are currently not subject to the garbage-collector,
-- and so need to be explicitly freed.   The following rules for this
-- should be observed.
--
-- (1) For writeBytes, it is only guaranteed that the argument "Bytes"
--     will be valid at the actual time of evaluation.
-- (2) For readBytes, it is the caller's responsibility to free the returned
--     area.
-- ----------------------------------------------------------------------

data WriteBinary m =
   WriteBinary {
      writeByte :: Byte -> m (),
      writeBytes :: Bytes -> Int -> m ()
      }

data ReadBinary m =
   ReadBinary {
      readByte :: m Byte,
      readBytes :: Int -> m Bytes
      }

class HasBinary a m where
   writeBin :: WriteBinary m -> a -> m ()
   readBin :: ReadBinary m -> m a
   
-- ----------------------------------------------------------------------
-- Reading/Writing HasBinary instances to Handles.
-- ----------------------------------------------------------------------

hWrite :: HasBinary a IO => Handle -> a -> IO ()
hWrite handle a = writeBin (toWriteBinaryHandle handle) a

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

-- ----------------------------------------------------------------------
-- Writing HasBinary instances to a memory area
--
-- We do this by allocating an area, and then doubling its size as
-- necessary.
-- ----------------------------------------------------------------------

data BinArea = BinArea {
   bytes :: ! Bytes, -- current storage area
   len :: ! Int, -- its length
   next :: ! Int -- where to write next bit of data.
   }

writeToBytes :: HasBinary a StateBinArea => a -> IO (Bytes,Int)
writeToBytes = writeToBytes0 1000
   -- Be generous, since memory is cheap.  Make it a bit less than a power
   -- of two, since some memory allocation algorithms (buddy algorithm)
   -- like this.

writeToBytes0 :: HasBinary a StateBinArea => Int -> a -> IO (Bytes,Int)
-- The integer gives the initial size to allocate.  It is probably better that
-- this be an overestimate (unless memory is very tight).
-- 
-- The result is returned as a pair (data area,length)
writeToBytes0 len0 a =
   do
      binArea0 <- mkEmptyBinArea len0
      ((),binArea1) <- runStateT (writeBin writeBinaryBinArea a) binArea0
      closeBinArea binArea1

mkEmptyBinArea :: Int -> IO BinArea 
-- the argument gives the initial size to use (which had better be positive).
mkEmptyBinArea len =
   do
      bytes <- bytesMalloc len 
      return (BinArea {
         bytes = bytes,
         len = len,
         next = 0
         })

closeBinArea :: BinArea -> IO (Bytes,Int)
closeBinArea binArea =
   do
      let
         bytes1 = bytes binArea
         len = next binArea
      bytes2 <- bytesReAlloc bytes1 len 
      return (bytes2,len)

type StateBinArea = StateT BinArea IO -- a state monad containing the BinArea.

writeBinaryBinArea :: WriteBinary StateBinArea
writeBinaryBinArea = WriteBinary {
   writeByte = (\ byte ->
      StateT (\ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + 1
            binArea1 <- ensureBinArea binArea0 next1
            putByteToBytes byte (bytes binArea1) next0
            return ((),binArea1 {next = next1})
         )
      ),
   writeBytes = (\ bytes' len ->
      StateT (\ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + len
            binArea1 <- ensureBinArea binArea0 next1
            putBytesToBytes bytes' 0 (bytes binArea1) next0 len
            return ((),binArea1 {next = next1})
         )
      )
   }

ensureBinArea :: BinArea -> Int -> IO BinArea
-- ensure that the given BinArea can hold at least len bytes.
ensureBinArea binArea size =
   if size <= len binArea
      then
         return binArea
      else
        do
           let
              len1 = 2*size
           bytes1 <- bytesReAlloc (bytes binArea) len1
           return (BinArea {
              bytes = bytes1,
              len = len1,
              next = next binArea
              })
   
-- ----------------------------------------------------------------------
-- Reading Binary instances from a memory area
-- We use BinArea's for this too.  But this is simpler, because we don't have to
-- worry about reallocing.
-- ----------------------------------------------------------------------

readFromBytes :: HasBinary a StateBinArea => (Bytes,Int) -> IO a
readFromBytes (bl@(bytes',len')) =
   do
      let
         binArea0 = mkBinArea bl

      (a,binArea1) <- runStateT (readBin readBinaryBinArea) binArea0
      checkFullBinArea binArea1
      return a

mkBinArea :: (Bytes,Int) -> BinArea
mkBinArea (bytes',len') = 
   BinArea {
      bytes = bytes',
      len = len',
      next = 0
      }

checkFullBinArea :: BinArea -> IO ()
checkFullBinArea binArea =
   if next binArea == len binArea 
      then
         return ()
      else
         error "Binary.checkFullBinArea: mysterious extra bytes"
   

readBinaryBinArea :: ReadBinary StateBinArea
readBinaryBinArea = ReadBinary {
   readByte = StateT (\ binArea0 ->
      do
         let
            next0 = next binArea0
            next1 = next0 + 1
         checkBinArea binArea0 next1
         byte <- getByteFromBytes (bytes binArea0) next0
         return (byte,binArea0 {next = next1})
      ),
   readBytes = (\ len -> 
      StateT (\ binArea0 ->
         do
            let
               next0 = next binArea0
               next1 = next0 + len
            checkBinArea binArea0 next1
            bytes' <- bytesMalloc len
            putBytesToBytes (bytes binArea0) next0 bytes' 0 len
            return (bytes',binArea0 {next = next1})
         )
      )
   }

checkBinArea :: BinArea -> Int -> IO ()
-- check that the given BinArea can hold at least len bytes.
checkBinArea binArea newNext =
   if newNext > len binArea
      then
         error "Binary.checkBinArea - BinArea overflow on read"
      else
         return ()

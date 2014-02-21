{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

-- | This module provides immutable CStrings, which additionally have
-- the property that they are automatically freed when the garbage-collector
-- forgets about them.
module Util.ICStringLen(
   ICStringLen, -- instance of AtomString and Eq.

   UTF8(..),
      -- newtype alias.  UTF8 ICStringLen is also an instance of AtomString,
      -- but we assume the characters are UTF8-encoded.
   toUTF8,
      -- :: String -> String
   fromUTF8WE,
      -- :: String -> WithError String
      -- This can be used for an error-checking UTF8 conversion.

   -- general creation and reading.
   mkICStringLen, -- :: Int -> (Ptr CChar -> IO()) -> IO ICStringLen
   mkICStringLenExtra,
      -- :: Int -> (CString -> IO extra) -> IO (ICStringLen,extra)
   withICStringLen, -- :: ICStringLen -> (Int -> Ptr CChar -> IO a) -> IO a


   -- Conversion to/from (Bytes,Int)
   -- NB.  Once a bytes value is converted to an ICStringLen,
   -- that ICStringLen will automatically free the pointer when the
   -- ICStringLen value is garbage collected.
   bytesToICStringLen, -- :: (Bytes,Int) -> IO ICStringLen
   bytesFromICStringLen, -- :: ICStringLen -> (Bytes,Int)
   touchICStringLen, -- :: ICStringLen -> IO ()

   -- Conversion to and from other objects
   readICStringLen, -- :: HasBinary a StateBinArea => ICStringLen -> IO a
   writeToICStringLen, -- :: HasBinary a StateBinArea => a -> IO ICStringLen


   ) where

import System.IO.Unsafe
import Foreign.C.String
import Foreign.ForeignPtr
#if __GLASGOW_HASKELL__ > 706
import Foreign.ForeignPtr.Unsafe
#endif
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Control.Monad.Trans

import Util.AtomString
import Util.Bytes
import Util.Binary
import Util.Computation
import Util.ExtendedPrelude
import Util.Dynamics
import Util.UTF8

-- ------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------

data ICStringLen = ICStringLen (ForeignPtr CChar) Int deriving (Typeable)

newtype UTF8 bytes = UTF8 bytes

-- ------------------------------------------------------------------
-- Creation and reading
-- ------------------------------------------------------------------

instance StringClass ICStringLen where
   fromString str = unsafePerformIO (innerFromString str)
      where
         innerFromString :: String -> IO ICStringLen
         innerFromString str =
            do
               let
                  len = length str
               mkICStringLen len
                  (\ ptr -> pokeArray ptr
                     (map castCharToCChar str)
                     )

   toString icsl = unsafePerformIO (innerToString icsl)
      where
         innerToString :: ICStringLen -> IO String
         innerToString icsl =
            withICStringLen icsl
               (\ len ptr ->
                  do
                     cchars <- peekArray len ptr
                     return (map castCCharToChar cchars)
                  )


instance StringClass (UTF8 ICStringLen) where
   fromString str = UTF8 (fromString (toUTF8 str))
   toString (UTF8 icsl) = coerceWithError (fromUTF8WE (toString icsl))

instance Show ICStringLen where
   show = show . toString

-- -------------------------------------------------------------------
-- General functions for Creating and reading ICStringLen's.
-- -------------------------------------------------------------------

mkICStringLen :: Int -> (CString -> IO()) -> IO ICStringLen
mkICStringLen len writeFn =
   do
      ptr <- mallocArray len
      writeFn ptr
      createICStringLen ptr len

mkICStringLenExtra :: Int -> (CString -> IO extra) -> IO (ICStringLen,extra)
mkICStringLenExtra len writeFn =
   do
      ptr <- mallocArray len
      extra <- writeFn ptr
      icsl <- createICStringLen ptr len
      return (icsl,extra)

withICStringLen :: ICStringLen -> (Int -> CString -> IO a) -> IO a
withICStringLen (ICStringLen foreignPtr len) readFn =
   withForeignPtr foreignPtr (\ ptr -> readFn len ptr)

createICStringLen :: CString -> Int -> IO ICStringLen
createICStringLen ptr len =
   do
      foreignPtr <- newForeignPtr finalizerFree ptr
      return (ICStringLen foreignPtr len)


-- -------------------------------------------------------------------
-- Converting ICStringLen directly to its components.
-- -------------------------------------------------------------------

bytesToICStringLen :: (Bytes,Int) -> IO ICStringLen
bytesToICStringLen (bytes,i) = createICStringLen (unMkBytes bytes) i

bytesFromICStringLen :: ICStringLen -> (Bytes,Int)
bytesFromICStringLen (ICStringLen foreignPtr len)
   = (mkBytes (unsafeForeignPtrToPtr foreignPtr),len)

touchICStringLen :: ICStringLen -> IO ()
touchICStringLen (ICStringLen foreignPtr _) = touchForeignPtr foreignPtr

-- -------------------------------------------------------------------
-- Instance of EqIO, OrdIO.
-- -------------------------------------------------------------------

instance OrdIO ICStringLen where
   compareIO (ICStringLen fptr1 len1) (ICStringLen fptr2 len2) =
      case compare len1 len2 of
         LT -> return LT
         GT -> return GT
         EQ -> compareBytes (mkBytes (unsafeForeignPtrToPtr fptr1))
            (mkBytes (unsafeForeignPtrToPtr fptr2)) len1

instance EqIO ICStringLen where
   eqIO icsl1 icsl2 =
      do
         ord <- compareIO icsl1 icsl2
         return (ord == EQ)

-- -------------------------------------------------------------------
-- Oh very well.  We implement Eq for ICStringLen, using unsafePerformIO.
-- -------------------------------------------------------------------

instance Eq ICStringLen where
   (==) icsl1 icsl2 = unsafePerformIO (eqIO icsl1 icsl2)

-- -------------------------------------------------------------------
-- Instance of HasBinary
-- -------------------------------------------------------------------

instance MonadIO m => HasBinary ICStringLen m where
   writeBin wb icsl =
      do
         r <- writeBin wb (bytesFromICStringLen icsl)
         seq r done
         liftIO (touchICStringLen icsl)
         return r
   readBin rb =
      do
         bl <- readBin rb
         icsl <- liftIO (bytesToICStringLen bl)
         return icsl

-- -------------------------------------------------------------------
-- Conversion to and from other objects
-- -------------------------------------------------------------------

readICStringLen :: HasBinary a StateBinArea => ICStringLen -> IO a
readICStringLen icsl =
   do
      let
         bl = bytesFromICStringLen icsl

      a <- readFromBytes bl
      touchICStringLen icsl
      return a

writeToICStringLen :: HasBinary a StateBinArea => a -> IO ICStringLen
writeToICStringLen a =
   do
      bl <- writeToBytes a
      bytesToICStringLen bl

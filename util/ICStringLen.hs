{- This module provides immutable CStrings, which additionally have
   the property that they are automatically freed when the garbage-collector
   forgets about them.
   -}
module ICStringLen(
   ICStringLen, -- instance of AtomString

   UTF8(..), 
      -- newtype alias.  UTF8 ICStringLen is also an instance of AtomString,
      -- but we assume the characters are UTF8-encoded.

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

   ) where

import Char

import System.IO.Unsafe
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Control.Monad.Trans
import Data.Bits
import AtomString

import Bytes
import Binary
import Computation(done)
import BinaryInstances
import ExtendedPrelude
import Dynamics
import CompileFlags
import TemplateHaskellHelps

-- ------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------

data ICStringLen = ICStringLen (ForeignPtr CChar) Int deriving (Typeable)

newtype UTF8 bytes = UTF8 bytes

-- -------------------------------------------------------------------
-- Extracting a ForeignPtr's components.
-- -------------------------------------------------------------------

$(
   if ghcShortVersion <= 601
      then
         [d|
            unsafeForeignPtrToPtr = $(dynName "foreignPtrToPtr")
         |]
      else
         [d|
            template = "haskell" -- null declaration for now
         |]
   )

-- ------------------------------------------------------------------
-- Creating a newForeignPtr (602 style)
-- ------------------------------------------------------------------

$(
   if ghcShortVersion <= 601
      then
         [d|
            newForeignPtr0 finalizerLen ptr 
               = $(dynName "newForeignPtr") ptr finalizerLen
         |]
      else
         [d|
            newForeignPtr0 = $(dynName "newForeignPtr")
         |]
   )

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
   fromString str = UTF8 (fromString (toUTF str))
   toString (UTF8 icsl) = fromUTF (toString icsl)

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
      foreignPtr <- newForeignPtr0 finalizerFree ptr
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
-- Unicode conversion
-- (code stolen from Axel Simon's post):
--    http://www.haskell.org/pipermail/haskell/2003-July/012252.html
-- -------------------------------------------------------------------

-- Convert Unicode characters to UTF-8.
--
toUTF :: String -> String
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = x:toUTF xs
             | ord x<=0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
                               chr (0x80 .|. (ord x .&. 0x3F)):
                               toUTF xs
             | otherwise     = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
                               chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
                               chr (0x80 .|. (ord x .&. 0x3F)):
                               toUTF xs

-- Convert UTF-8 to Unicode.
--
fromUTF :: String -> String
fromUTF [] = []
fromUTF (all@(x:xs)) | ord x<=0x7F = x:fromUTF xs
                     | ord x<=0xBF = err
                     | ord x<=0xDF = twoBytes all
                     | ord x<=0xEF = threeBytes all
                     | otherwise   = err
  where
    twoBytes (x1:x2:xs) = chr (((ord x1 .&. 0x1F) `shift` 6) .|.
                               (ord x2 .&. 0x3F)):fromUTF xs
    twoBytes _ = error "Unicode decode: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr (((ord x1 .&. 0x0F) `shift` 12) .|.
                                    ((ord x2 .&. 0x3F) `shift` 6) .|.
                                    (ord x3 .&. 0x3F)):fromUTF xs
    threeBytes _ = error "Unicode decode: illegal three byte sequence" 
    
    err = error "Unicode decode: illegal UTF-8 character"




{- This module provides immutable CStrings, which additionally have
   the property that they are automatically freed when the garbage-collector
   forgets about them.
   -}
module ICStringLen(
   ICStringLen, -- instance of AtomString

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

import System.IO.Unsafe
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Control.Monad.Trans

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
      foreignPtr <- newForeignPtr ptr finalizerFree
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

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

   ) where

import System.IO.Unsafe
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.Types

import AtomString

-- ------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------

data ICStringLen = ICStringLen (ForeignPtr CChar) Int

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
      foreignPtr <- newForeignPtr ptr (free ptr)
      return (ICStringLen foreignPtr len)

mkICStringLenExtra :: Int -> (CString -> IO extra) -> IO (ICStringLen,extra)
mkICStringLenExtra len writeFn =
   do
      ptr <- mallocArray len
      extra <- writeFn ptr
      foreignPtr <- newForeignPtr ptr (free ptr)
      return (ICStringLen foreignPtr len,extra)

withICStringLen :: ICStringLen -> (Int -> CString -> IO a) -> IO a
withICStringLen (ICStringLen foreignPtr len) readFn =
   withForeignPtr foreignPtr (\ ptr -> readFn len ptr)

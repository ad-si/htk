#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC 
{-# OPTIONS -#include "copy_file.h" #-}
#endif /* NEW_GHC */

{- This contains two functions for copying to a file -}
module CopyFile(
   copyFile,
   linkFile,
   copyStringToFile,
   copyFileToString,
   ) where

import PackedString
import CTypesISO(CSize)
import ST
import ByteArray(ByteArray)
import qualified IOExts
import qualified CString
import qualified Ptr
import qualified MarshalAlloc
import qualified Posix
import Exception

import Computation

#ifdef NEW_GHC
foreign import ccall unsafe "copy_file.h copy_file" copyFilePrim 
   :: (ByteArray Int) -> (ByteArray Int) -> IO Int

foreign import ccall unsafe "copy_file.h copy_string_to_file" 
   copyStringToFilePrim 
      :: CSize -> (ByteArray Int) -> (ByteArray Int) -> IO Int

#else
foreign import ccall "copy_file" unsafe copyFilePrim
   :: (ByteArray Int) -> (ByteArray Int) -> IO Int

foreign import ccall "copy_string_to_file" unsafe copyStringToFilePrim 
      :: CSize -> (ByteArray Int) -> (ByteArray Int) -> IO Int

#endif

copyFile :: String -> String -> IO ()
copyFile source destination =
   if source == destination 
      then
         done
      else
         do
            let 
               sourcePrim = CString.packString source
               destinationPrim = CString.packString destination 
            code <- copyFilePrim sourcePrim destinationPrim
            if (code<0)
               then
                  ioError(userError("CVSDB: Can't copy "++source++" to "++
                     destination++" with error "++show code))
               else
                  return ()

---
-- At the moment this does a hard link.  We should perhaps consider
-- letting it do a soft link instead when a hard link is not possible
linkFile :: String -> String -> IO ()
linkFile source destination =
   if source == destination 
      then
         done
      else
         do
            success <- Exception.try (Posix.createLink source destination)
            case success of
               Right () -> done
               Left err ->
                  error ("CopyFile.linkFile failed with "++show err)

copyStringToFile string destination = writeFile destination string

-- This is a direct encapsulatin of copyStringToFile in C
-- which we turn out not to need, yet.
copyStringToFileAlternative string destination =
   do
      let
         packedString = PackedString.packString string
         packedBytes = psToByteArray packedString
         (sizetLen :: CSize) = fromIntegral (lengthPS packedString)  
         destinationPrim = CString.packString destination 
      code <- copyStringToFilePrim sizetLen packedBytes destinationPrim
      if (code<0)
         then
            ioError(userError("CVSDB: Can't copy string to "++
               destination++" with error "++show code))
         else
            return ()

---
-- Reads in a file to a String.  NB - differs from readFile in that this
-- is done instantly, so we don't have to worry about semi-closed handles
-- hanging around.
copyFileToString :: FilePath -> IO String
copyFileToString file =
   do
      (ptr,len) <- IOExts.slurpFile file
      str <- CString.peekCStringLen (Ptr.castPtr ptr,len)
      MarshalAlloc.free ptr
      return str

{-
   Another way


copyFileToString :: String -> IO String
copyFileToString file =
   do
      handle <- openFile file ReadMode
      contents <- hGetContents handle
      seq (last contents) (hClose handle)
      -- The seq hopefully forces everything to be read.
      -- PS I've tried removing seq on the grounds that
      -- hClose should make it unnecessary, but it breaks
      -- the Versions test on Linux.
      return contents

-}      



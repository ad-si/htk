#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC 
{-# OPTIONS -#include "copy_file.h" #-}
#endif /* NEW_GHC */

{- This contains functions for copying to and from files -}
module CopyFile(
   copyFile,
   linkFile,
   copyStringToFile,
   copyFileToString,

   copyCStringLenToFile,
   copyFileToCStringLen,
   ) where

import qualified IO

import PackedString
import CTypesISO(CSize)
import ST
import ByteArray(ByteArray)
import qualified IOExts
import qualified Exception
import CString
import Ptr
import Foreign
import Posix

import Computation

import FdRead

#ifdef NEW_GHC
foreign import ccall unsafe "copy_file.h copy_file" copyFilePrim 
   :: (ByteArray Int) -> (ByteArray Int) -> IO Int
#else
foreign import ccall "copy_file" unsafe copyFilePrim
   :: (ByteArray Int) -> (ByteArray Int) -> IO Int
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

---
-- Reads in a file to a String.  NB - differs from readFile in that this
-- is done instantly, so we don't have to worry about semi-closed handles
-- hanging around.
copyFileToString :: FilePath -> IO String
copyFileToString filePath =
   do
      (cString,len) <- copyFileToCStringLen filePath
      string <- peekCStringLen (cString,len)
      free cString
      return string

copyFileToCStringLen :: FilePath -> IO CStringLen
copyFileToCStringLen file =
#if __GLASGOW_HASKELL__ <= 503
-- We catch ioErrors which don't match certain criteria and return 
-- the null string.  This is because IOExts.slurpFile fails on files 
-- of zero length with an IOError which doesn't pass any of the
-- standard test functions
   do
      let
         selector ex =
            case Exception.ioErrors ex of
               Nothing -> Nothing
               Just (ioError :: IOError) ->
                  let
                     testFuns = [
                        IO.isAlreadyExistsError,
                        IO.isDoesNotExistError,
                        IO.isAlreadyInUseError,
                        IO.isFullError,
                        IO.isEOFError,
                        IO.isIllegalOperation,
                        IO.isPermissionError,
                        IO.isUserError
                        ]
                     isStandard = any id (map ($ ioError) testFuns)
                  in
                     if isStandard then Nothing else Just ()

      Exception.catchJust selector 
         (copyFileToCStringLen' file) (\ _ -> newCStringLen "")

copyFileToCStringLen' :: FilePath -> IO CStringLen
copyFileToCStringLen' file =      
#endif
   do
      (ptr,len) <- IOExts.slurpFile file
      return (Ptr.castPtr ptr,len)

copyStringToFile :: String -> FilePath -> IO ()
copyStringToFile str filePath =
   withCStringLen str 
      (\ cStringLen -> copyCStringLenToFile cStringLen filePath)

copyCStringLenToFile :: CStringLen -> FilePath -> IO ()
copyCStringLenToFile (ptr,len) filePath =
   do
      let
         fileMode = unionFileModes ownerReadMode ownerWriteMode
         openFileFlags = OpenFileFlags {
            append = False,
            exclusive = False,
            noctty = True,
            nonBlock = True,
            trunc = True
            }

      fd <- openFd filePath WriteOnly (Just fileMode) openFileFlags 
      fdWritePrim fd (ptr,len)
      fdClose fd



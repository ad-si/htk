{- This contains functions for copying to and from files -}
module CopyFile(
   copyFile,
   linkFile,
   copyStringToFile,
   copyStringToFileCheck,
   copyFileToString,
   copyFileToStringCheck,

   copyCStringLenToFile,
   copyFileToCStringLen,
   copyFileToICStringLen,
   ) where

import qualified IO

import GHC.IO
import CTypesISO(CSize)
import ST
import qualified IOExts
import qualified Exception
import CString
import Ptr
import Foreign
import Posix

import Computation
import ICStringLen

import FdRead

foreign import ccall unsafe "copy_file.h copy_file" copyFilePrim 
   :: CString -> CString -> IO Int

copyFile :: String -> String -> IO ()
copyFile source destination =
   if source == destination 
      then
         done
      else
         do
            code <-
               withCString source (\ sourcePrim ->
                  withCString destination (\ destinationPrim ->
                     copyFilePrim sourcePrim destinationPrim
                     )
                  )
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

---
-- Read in a file, catching certain errors 
copyFileToStringCheck :: FilePath -> IO (WithError String)
copyFileToStringCheck filePath =
   exceptionToError
      (\ exception ->
         case Exception.ioErrors exception of
         Nothing -> Nothing
         Just ioError ->
            if IO.isDoesNotExistError ioError
            then
               Just "File does not exist"
            else if IO.isAlreadyInUseError ioError
            then
               Just "File is already in use"
            else if IO.isPermissionError ioError
            then
               Just "No read access to file"
            else
               Nothing     
         )
      (copyFileToString filePath)

copyFileToCStringLen :: FilePath -> IO CStringLen
copyFileToCStringLen file =
   do
      (ptr,len) <- IOExts.slurpFile file
      return (Ptr.castPtr ptr,len)

copyFileToICStringLen :: FilePath -> IO ICStringLen
copyFileToICStringLen filePath =
   do
      -- shamelessly pirated from GHC's slurpFile function.
      handle <- IO.openFile filePath IO.ReadMode
      len <- IO.hFileSize handle
      if len > fromIntegral (maxBound::Int) 
         then
            error "CopyFile.copyFileToICStringLen: file too big" 
         else
            do
               let
                  len_i = fromIntegral len
               mkICStringLen len_i
                  (\ cString -> 
                     do
                        lenRead <- hGetBuf handle cString len_i
                        when (lenRead < len_i)
                           (error"EOF within BinaryIO")
                     )

---
-- Write to a file, catching certain errors.
-- (At the moment this is not very helpful, returning messages like
-- "system error").
copyStringToFileCheck :: String -> FilePath -> IO (WithError ())
copyStringToFileCheck str filePath =
   exceptionToError
      (\ exception ->
         case Exception.ioErrors exception of
            Nothing -> Nothing
            Just ioError -> Just (show ioError) 
         )
      (copyStringToFile str filePath)

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



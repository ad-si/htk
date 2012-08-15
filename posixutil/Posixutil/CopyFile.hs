-- | This contains functions for copying to and from files
module Posixutil.CopyFile(
   copyFile,
   copyFileWE,
   copyStringToFile,
   copyStringToFileCheck,
   copyFileToString,
   copyFileToStringCheck,
   copyICStringLenToFile,
   copyFileToICStringLenCheck,

   copyCStringLenToFile,
   copyFileToCStringLen,
   copyFileToICStringLen,
   ) where

import System.IO as IO
import System.IO.Error as IO
import qualified System.IO.Error as IOErr

import Foreign.C
import Control.Exception as Exception
import qualified System.Directory as Dir

import Util.Computation
import Util.ICStringLen
import Util.DeepSeq

-- amahnke: Supplemented C-Code by System.Directory.CopyFile:
--
-- foreign import ccall unsafe "copy_file.h copy_file" copyFilePrim
--   :: CString -> CString -> IO Int

copyFile :: String -> String -> IO ()
copyFile source destination =
   do
      unitWE <- copyFileWE source destination
      coerceWithErrorIO unitWE

copyFileWE :: String -> String -> IO (WithError ())
copyFileWE source destination =
   if source == destination
      then
         return (hasValue ())
      else
             Exception.catch (do
                            Dir.copyFile source destination
                            return(hasValue()))
              (\ioErr ->
                  let
                     codeStr = if IOErr.isAlreadyExistsError ioErr
                                 then  ("Can't write to " ++ destination ++ ". File already exists!")
                                 else if IOErr.isDoesNotExistError ioErr
                                        then ("Can't read from " ++ source ++ ". File doesn't exists!")
                                        else if IOErr.isPermissionError ioErr
                                               then ("Can't write to " ++ destination ++ ". Insufficient permissions!")
                                               else if IOErr.isFullError ioErr
                                                      then ("Can't write to " ++ destination ++ ". Disk full!")
                                                      else ("Something went wrong within copyFile.")
                  in
                    return(hasError codeStr)
              )
 {--
           code <-
               withCString source (\ sourcePrim ->
                  withCString destination (\ destinationPrim ->
                     copyFilePrim sourcePrim destinationPrim
                     )
                  )
            if (code<0)
               then
                  let
                     codeStr = case code of
                        -- see includes/copy_file.h
                        -1 -> "Can't read from "++source
                        -2 -> "Can't write to "++destination
                        -3 -> "Not enough memory to allocate buffer"
                        _ -> "Unknown error!!"
                  in
                     return(hasError codeStr)
               else
                  return(hasValue ())
--}

-- | Reads in a file to a String.  NB - differs from readFile in that this
-- is done instantly, so we don\'t have to worry about semi-closed handles
-- hanging around.
copyFileToString :: FilePath -> IO String
copyFileToString filePath =
   do
      s <- IO.readFile filePath

--      (cString,len) <- copyFileToCStringLen filePath
--      string <- peekCStringLen (cString,len)
--      free cString
      s `deepSeq` (return s)

-- | Read in a file, catching certain errors
copyFileToStringCheck :: FilePath -> IO (WithError String)
copyFileToStringCheck filePath =
   exceptionToError
      (\ ioError ->
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
      str <- readFile file
      newCStringLen $!! str

copyFileToICStringLenCheck :: FilePath -> IO (WithError ICStringLen)
copyFileToICStringLenCheck filePath =
   exceptionToError
      (\ ioError -> Just $ show (ioError :: IOException)
         )
      (copyFileToICStringLen filePath)

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
                           (error"EOF within CopyFile.copyFileToICStringLen")
                     )

copyICStringLenToFile :: ICStringLen -> FilePath -> IO ()
copyICStringLenToFile icsl filePath =
   withICStringLen icsl (\ i cstr ->
      copyCStringLenToFile (cstr,i) filePath
      )

-- | Write to a file, catching certain errors.
-- (At the moment this is not very helpful, returning messages like
-- \"system error\").
copyStringToFileCheck :: String -> FilePath -> IO (WithError ())
copyStringToFileCheck str filePath =
   exceptionToError
      (\ ioError -> Just $ show (ioError :: IOException)
         )
      (copyStringToFile str filePath)

copyStringToFile :: String -> FilePath -> IO ()
copyStringToFile str filePath =
   withCStringLen str
      (\ cStringLen -> copyCStringLenToFile cStringLen filePath)

copyCStringLenToFile :: CStringLen -> FilePath -> IO ()
copyCStringLenToFile (ptr,len) filePath =
   do
      handle <- IO.openFile filePath IO.WriteMode
      hPutBuf handle ptr len
      IO.hFlush handle

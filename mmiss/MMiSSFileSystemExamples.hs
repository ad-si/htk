-- | Module which contains various examples of LaTeXParser.FileSystem. 
module MMiSSFileSystemExamples(
   oneFileFileSystem, -- :: FilePath -> String -> FileSystem
   standardFileSystem, -- :: FileSystem
   ) where

import Control.Concurrent.MVar

import Computation

import CopyFile

import LaTeXParser
import LaTeXPreamble

-- --------------------------------------------------------------------
-- oneFileFileSystem
-- --------------------------------------------------------------------

oneFileFileSystem :: FilePath -> String -> FileSystem
oneFileFileSystem fPath contents =
   let
      readString fPath2 
         | fPath == fPath2 
            = return (hasValue contents)
         | True 
            = return (hasError ("Attempt to read file " ++
               fPath2 ++ " that is not available"))
      writeString _ _ = return (hasError (
         "Unexpected attempt to write when parsing a document"))
   in
      FileSystem {readString = readString,writeString = writeString}

-- --------------------------------------------------------------------
-- oneWritableFileFileSystem
-- --------------------------------------------------------------------

oneWritableFileFileSystem :: FilePath -> IO FileSystem
oneWritableFileFileSystem filePath0 =
   do
      (contentsMVar :: MVar (Maybe String)) <- newMVar Nothing
      let
         writeString1 contents filePath1 =
            if filePath0 == filePath1 
               then
                  do
                     swapMVar contentsMVar (Just contents)
                     return (hasValue ())
               else
                  return (hasError ("Cannot write file with name " ++ filePath1
                     ++ " to file system expecting name " ++ filePath0))
         readString1 filePath1 =
            if filePath0 == filePath1 
               then
                  do
                     contentsOpt <- readMVar contentsMVar
                     case contentsOpt of
                        Just contents -> return (hasValue contents)
                        Nothing -> return (hasError ("File " ++ filePath1 
                           ++ " has not been written yet"))
               else
                  return (hasError ("Cannot read file with name " ++ filePath1
                     ++ " from file system only containing name " 
                     ++ filePath0))

      return (FileSystem {readString = readString1,writeString = writeString1})

-- --------------------------------------------------------------------
-- The standard file system
-- --------------------------------------------------------------------

standardFileSystem :: FileSystem 
standardFileSystem = FileSystem {
   readString = copyFileToStringCheck,
   writeString = copyStringToFileCheck
   }
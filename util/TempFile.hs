-- | The TempFile module allocates temporary files 
module TempFile(
   newTempFile, -- :: IO FilePath
   ) where


import Directory

import Concurrent
import qualified IOExts(unsafePerformIO)

import IOExtras
import WBFiles
import UniqueFile
import FileNames

data TempFileSource = TempFileSource {
   fileStore :: UniqueFileStore,
   fileSource :: MVar UniqueFileCounter
   }

tempFileSource :: TempFileSource
tempFileSource = IOExts.unsafePerformIO (
   do
      workingDir <- getWorkingDir
      let directory = combineNames workingDir "#"
      catchAlreadyExists (createDirectory workingDir)
      catchAlreadyExists (createDirectory directory)
      fileStore <- newUniqueFileStore directory createDirectory
      fileSource <- newMVar initialUniqueFileCounter
      return (TempFileSource {fileStore = fileStore,fileSource = fileSource})
   )
{-# NOINLINE tempFileSource #-}

newTempFile :: IO FilePath
newTempFile =
   do
      let 
         TempFileSource {fileStore = fileStore,fileSource = fileSource} =
            tempFileSource
      fileCounter <- takeMVar fileSource
      let (newName,nextFileCounter) = stepUniqueFileCounter fileCounter
      putMVar fileSource nextFileCounter
      ensureDirectories fileStore newName
      return (getFilePath fileStore newName)
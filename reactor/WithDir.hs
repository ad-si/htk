-- | A function for changing directories in a thread-safe way.
-- 
-- We use an MSem to lock the current directory.  This means that
-- withDir can be nested without deadlock (presumably the user knows what
-- he's doing).
module WithDir(
   withDir, -- :: FilePath -> IO a -> IO a
   ) where

import Directory

import System.IO.Unsafe

import Computation

import Synchronized

import MSem

dirMSem :: MSem
dirMSem = unsafePerformIO newMSem
{-# NOINLINE dirMSem #-}


withDir :: FilePath -> IO a -> IO a
withDir filePath act =
   synchronize dirMSem (
      do
         originalDir <- getCurrentDirectory
         setCurrentDirectory filePath
         actOut <- try act
         setCurrentDirectory originalDir
         propagate actOut
      )
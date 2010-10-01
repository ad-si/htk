-- | A function for changing directories in a thread-safe way.
--
-- We use an MSem to lock the current directory.  This means that
-- withDir can be nested without deadlock (presumably the user knows what
-- he's doing).
module Reactor.WithDir(
   withDir, -- :: FilePath -> IO a -> IO a
   ) where

import System.Directory

import System.IO.Unsafe

import Util.Computation

import Events.Synchronized

import Reactor.MSem

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

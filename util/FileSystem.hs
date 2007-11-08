-- | A FileSystem is a space on disk (or wherever files are kept) with a
-- function (ensureFiles) that extracts files via a given action
-- on demand, creating directories as required.
module FileSystem(
   FileSystem,
   FileSystemName,
   newFileSystem,
   HasFileSystem(..),
   getFileSystemNameLocation,
   ensureFile,
   ensureFiles,
   ) where

import Directory

import Computation(done)
import TempFile
import FileNames
import Registry
import IOExtras

-- ----------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------

-- | This corresponds to a file name within the file system, with the top
-- directory first.
type FileSystemName = [String]

data FileSystem = FileSystem {
   location :: FilePath, -- Where the file system is stored
   alreadyCopied :: LockedRegistry FileSystemName (),
      -- Files already in the file system
   existingDirs :: LockedRegistry String ()
      -- Directories already created
   }

-- ----------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------


-- | Create a new empty file system
newFileSystem :: IO FileSystem
newFileSystem =
   do
      location <- newTempFile
      alreadyCopied <- newRegistry
      existingDirs <- newRegistry

      -- insert the top directory
      catchAlreadyExists (createDirectory location)
      setValue existingDirs "" ()

      return (FileSystem {
         location = location,
         alreadyCopied = alreadyCopied,
         existingDirs = existingDirs
         })


class HasFileSystem fileSystem where
   getFileSystemLocation :: fileSystem -> FilePath

-- | Get the location of the directory containing the FileSystem.
instance HasFileSystem FileSystem where
   getFileSystemLocation (FileSystem {location = location}) = location

-- | Get the location corresponding to a particular file in the file system
-- (It doesn\'t ensure it is present; for that you need ensureFile\/ensureFiles).
getFileSystemNameLocation :: HasFileSystem fileSystem
   => fileSystem -> FileSystemName -> FilePath
getFileSystemNameLocation fileSystem fileSystemName =
   unbreakName ((getFileSystemLocation fileSystem):fileSystemName)

-- | Returns the parent directories strictly containing the given
-- file system name, from the immediate parent up to but not including
-- the root.
parentDirectories :: FileSystemName -> [FilePath]
parentDirectories parts =
   let
      pdReversed [] = []
      pdReversed [_] = []
      pdReversed (_:rest) =
         let
            pdRrest = pdReversed rest
         in
            (unbreakName (reverse rest)) : pdRrest
   in
      pdReversed (reverse parts)

-- | Ensure the directories containing a particular name are present.
ensureDirectories :: FileSystem -> FileSystemName -> IO ()
ensureDirectories fileSystem fileSystemName =
   do
      let
         containingDirs = parentDirectories fileSystemName
         doContainingDirs [] = done
         doContainingDirs (this:rest) =
            transformValue (existingDirs fileSystem) this
               (\ unitOpt -> case unitOpt of
                  Just () -> return (unitOpt,())
                  Nothing ->
                     do
                        doContainingDirs rest
                        createDirectory
                           (combineNames (location fileSystem) this)
                        return (Just (),())
                  )
      doContainingDirs containingDirs

-- | Ensure a particular file is present, calling the supplied action if
-- necessary.  This action should retrieve the object to the specified FilePath
-- (which will be the real location of the object).
ensureFile :: FileSystem -> (FileSystemName -> FilePath -> IO ())
   -> FileSystemName -> IO ()
ensureFile fileSystem extractFn fileSystemName =
   transformValue (alreadyCopied fileSystem) fileSystemName
      (\ unitOpt ->
         case unitOpt of
            Just () -> return (unitOpt,())
            Nothing ->
               do
                  ensureDirectories fileSystem fileSystemName
                  extractFn fileSystemName
                     (getFileSystemNameLocation fileSystem fileSystemName)
                  return (Just (),())
         )

-- | ensureFile for zero or more fileSystemNames.
ensureFiles :: FileSystem -> (FileSystemName -> FilePath -> IO ())
   -> [FileSystemName] -> IO ()
ensureFiles fileSystem extractFn fileSystemNames =
   mapM_ (ensureFile fileSystem extractFn) fileSystemNames

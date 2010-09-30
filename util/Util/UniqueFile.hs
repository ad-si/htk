-- | UniqueFile is used for allocating names for temporary files in a directory.
-- To avoid large numbers of files in the same directory, we create sub-
-- directories where necessary.
module Util.UniqueFile(
   UniqueFileCounter,
      -- This represents the state, which needs to be single-threaded.
      -- Instance of Read,Show so it can be transmitted.
   initialUniqueFileCounter, -- :: UniqueFileCounter
      -- This is how you start
   stepUniqueFileCounter, -- :: UniqueFileCounter -> (String,UniqueFileCounter)
      -- And this is how you get a String out.

   -- Here are some independent functions for actually managing the
   -- subdirectories.  We don't require that the file names be generated
   -- from a UniqueFileCounter.
   UniqueFileStore, -- This represents a location on disk where the
      -- unique files are actually stored.  NB - it is not expected that
      -- all files got from the unique file

   newUniqueFileStore,
      -- :: FilePath -> (FilePath -> IO ()) -> IO UniqueFileStore
      -- This creates a new file store.
      -- The FilePath should point do a directory, which must already
      -- exist.
      -- The user should specify the create-directory function in the
      -- second argument, which is assumed to work.  This is given
      -- the name relative to the top directory, not the full name.

   ensureDirectories,
      -- :: UniqueFileStore -> String -> IO ()
   -- ensureDirectories is given the relative location of a
   -- file inside the file store (../. characters not permitted!) and
   -- creates directories appropriately.

   getFilePath, -- :: UniqueFileStore -> String -> FilePath
   -- Get full name of a file in the unique file store.
   ) where

import System.Directory
import Data.Char

import Util.IOExtras
import Util.Registry
import Util.FileNames
import Util.Computation(done)

-- --------------------------------------------------------------
-- UniqueFileCounter
-- --------------------------------------------------------------

{-
   Strategy: each file name has the form
   [char]/[char]/.../[char]
   The [char] is chosen from the 64-character set:

   lower case and upper case letters (52)
   digits (10)
   @+

   Thus each char corresponds to a number between 0 and 63.
   The characters are divided into those with numbers <22
   and those with numbers >=22.  Characters with numbers >=22
   correspond to bits of the directory entry of the file name.
   The ones with numbers <22 correspond to the file name part.
   Thus the file names can get arbitrarily long.  The reason
   for choosing 22 is that it maximises the number of possibilities
   when there are up to three parts, which is 39754.
   -}

newtype UniqueFileCounter = UniqueFileCounter [Int] deriving (Show,Read)

initialUniqueFileCounter :: UniqueFileCounter
initialUniqueFileCounter = UniqueFileCounter [0]

stepUniqueFileCounter :: UniqueFileCounter -> (String,UniqueFileCounter)
stepUniqueFileCounter (UniqueFileCounter ilist) =
      (toString ilist,UniqueFileCounter (increment ilist))
   where
      toString :: [Int] -> String
      toString [] = error "UniqueFile.toString"
      toString (first:rest) = tS [encodeChar first] rest
         where
            tS :: String -> [Int] -> String
            tS acc [] = acc
            tS acc (first:rest) = tS ((encodeChar first):fileSep:acc) rest

      encodeChar :: Int -> Char
      encodeChar i=
         if i<26 then
            chr(ord 'a' + i)
         else if i<52 then
            chr((ord 'A'-26)+i)
         else if i<62 then
            chr((ord '0'-52)+i)
         else case i of
            62 -> '@'
            63 -> '+'
            _ -> error "UniqueFile.encodeChar"

      increment :: [Int] -> [Int]
      increment (file:rest) =
         if file==(divider-1)
            then
               0:(incrementDirs rest)
            else
               (file+1):rest
         where
            incrementDirs :: [Int] -> [Int]
            incrementDirs [] = [divider]
            incrementDirs (first:rest) =
               if first==(nChars-1)
                  then
                     divider:(incrementDirs rest)
                  else
                     (first+1):rest


      divider :: Int
      divider = 22

      nChars :: Int
      nChars = 64

-- --------------------------------------------------------------
-- UniqueFileStore
-- --------------------------------------------------------------

data UniqueFileStore = UniqueFileStore {
   directory :: FilePath, -- We trim a trailing slash, if any.
   alreadyExistsRegistry :: LockedRegistry String (),
      -- This is a cache of subdirectories already known to exist.
      -- Using a locked registry allows ensureDirectories to
      -- be run in several threads simultanesouly, without running concurrently
      -- on the same sub-directory.
   createDirAct :: FilePath -> IO ()
      -- function passed in by newUniqueFileStore
   }

newUniqueFileStore :: FilePath -> (FilePath -> IO ()) -> IO UniqueFileStore
newUniqueFileStore directory createDirAct =
   do
      exists <- doesDirectoryExist directory
      if exists
         then
            done
         else
            error "UniqueFile.newUniqueFileStore: directory must alreay exist"
      alreadyExistsRegistry <- newRegistry

      return (UniqueFileStore {
         directory = trimDir directory,
         createDirAct = createDirAct,
         alreadyExistsRegistry = alreadyExistsRegistry
         })

ensureDirectories :: UniqueFileStore -> String -> IO ()
ensureDirectories (uniqueFileStore @ UniqueFileStore {directory = directory,
      createDirAct = createDirAct,
      alreadyExistsRegistry = alreadyExistsRegistry}) fullName =
   case splitName fullName of
      (subDir,rest)
         | subDir == thisDir -> done -- no subdirectories required.
         | True ->
            transformValue alreadyExistsRegistry subDir
               (\ existsOpt ->
                  do
                     case existsOpt of
                        Just () -> -- no action required
                           done
                        Nothing ->
                           do
                              ensureDirectories uniqueFileStore subDir
                              catchAlreadyExists (createDirAct subDir)
                              done
                     return (Just (),())
                  )


getFilePath :: UniqueFileStore -> String -> FilePath
getFilePath (UniqueFileStore {directory = directory}) file =
   combineNames directory file

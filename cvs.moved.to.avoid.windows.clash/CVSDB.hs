{-# OPTIONS -#include "copy_file.h" #-}
{- CVSDB is the interface to CVS used by the rest of UniForM.
   The interface to this module is supposed to be standardised,
   meaning that other versioning systems can be substituted.
   -}
module CVSDB(
   Repository,
   RepositoryParameter(HostString,WorkingDir),
   -- ConnectionParameter contains various parameters required for
   -- establishing contact with the repository.  The hostString
   -- is a string of undefined format which represents the repository
   -- location.  The workingDir is a directory for the private
   -- use of the CVSDB module.
   -- While it should be avoided, ConnectionParameter is most likely to
   -- change if the versioning system changes.
   initialise, -- :: [RepositoryParameter] -> IO Repository
   -- Where the same parameter is specified multiple times, the first
   -- setting in the list is used.   

   ObjectVersion, 
   -- type of versions in the repository
   -- instance of Read and Show

   ObjectSource,
   -- type of data as retrieved from the repository.
   exportString, -- :: ObjectSource -> IO String
   -- exportString extracts the contents of the object as a String
   exportFile, -- :: ObjectSource -> FilePath -> IO ()
   -- exportFile writes the contents of the object as a file with the
   -- supplied name, overwriting whatever was there before.
   importString, -- :: String -> IO ObjectSource
   -- importString makes an object with the given contents.
   importFile, -- :: FilePath -> IO ObjectSource
   -- importFile makes an object from the given file.

   Location,
   -- represents Location of object in the repository.  Instance of Read/Show.
   newLocation, -- :: Repository -> ObjectSource -> 
      IO (Location,ObjectVersion)
   -- creates new object in the repository, returning its initial version.
   commit, -- :: Repository -> ObjectSource -> Location -> IO ObjectVersion
   -- commits a new version of the object to the repository, returning its
   -- new version.
   retrieve, -- :: Repository -> Location -> ObjectVersion -> IO ObjectSource
   -- This retrieves an object given the object version to retrieve.

   listVersions, -- :: Repository -> Location -> IO [ObjectVersion]
   -- listVersion lists all versions of the object with the given location.
   watch -- :: Repository -> Location -> IA ()
   -- watch returns an event which occurs whenever a commit occurs to the
   -- object with the given location.
   ) where

import System
import Directory

import FiniteMap
import qualified BSD

import Computation(done)
import Maybes

import Notification
import RegularExpression

import CVSHigh

data Repository = Repository {
   cvsLoc :: CVSLoc, -- CVSHigh location of repository.
   workingDir :: String, -- Working directory.  Includes terminal "/".
   wDirContents :: FiniteMap Location ObjectVersion,
      -- wDirContents contains all versions of objects currently in the
      -- working directory.
   notifier :: Notifier
   }

data RepositoryParameter =
      HostString String 
      -- For CVS, the CVSROOT parameter.  We also need to contact the
      -- notification server; this is assumed to exist on the same
      -- machine as the CVS server (for :pserver:) or on this machine,
      -- if CVSROOT is not :pserver:.
   |  WorkingDir String

----------------------------------------------------------------
-- Initialisation
----------------------------------------------------------------

initialise :: [RepositoryParameter] -> IO Repository
initialise options =
   do      
      workingDir <-
         case (firstJust 
            (map
               (\ option -> 
                  case option of 
                     WorkingDir workingDir -> Just workingDir
                     _ -> Nothing
                  ) 
               options
               )
            ) of
            Just workingDir -> return workingDir
            Nothing -> 
               error "CVSDB.initialise - must specify working directory"

      hostString <-
         case (firstJust 
            (map
               (\ option -> 
                  case option of 
                     HostString hostString -> Just hostString
                     _ -> Nothing
                  ) 
               options
               )
            ) of
            Just hostString -> return hostString
            Nothing -> getEnv "CVSROOT"

      cvsLoc <- newCVSLoc hostString workingDir

      host <- case getHost hostString of
         Just hostName -> hostName
         Nothing -> BSD.getHostName

      dirExists <- doesDirectoryExist workingDir
      if dirExists 
         then
            done
         else
            createDirectory workingDir
      setCurrentDirectory workingDir     
 
      cvsCheckout cvsLoc module >>>= checkReturn

      notifier <- mkNotifier hostName
      return Repository {
         cvsLoc = cvsLoc,
         workingDir = workingDir,
         wDirContents = emptyFM,
         notifier = notifier
         }
   where
      getHost :: String -> Maybe String
      -- getHost parses a String in CVSROOT format to extract the
      -- host name.  See cvs info page, section "The Repository".
      -- If getHost is Nothing that means the host is unspecified
      -- and so the local one.
      getHost hostString =
         case (breakColons hostString) of
            [[],"local",filename] -> Nothing
            [[],otherMethod,userHost,filename] ->
               case breakAt userHost of
                  Nothing -> cantParse
                  Just host -> Just host
            [userHost@(_:_),filename] ->
               case breakAt userHost of
                  Nothing -> cantParse
                  Just host -> Just host
            [local] -> Nothing
            _ -> cantParse
         where
            cantParse :: a
            cantParse = error ("Can't parse CVSROOT value: "++hostString)

       breakColons :: String -> [String]
      -- breakColons breaks a list by colons
      breakColons [] = [[]]
      breakColons (':':rest) = [] : (breakColons rest)
      breakColons (c:rest) = 
         case breakColons rest of
            head:rest -> (c:head):rest
      
      breakAt :: String -> Maybe String
      -- breakAt returns the result of breaking after the first @.
      breakAt [] = Nothing
      breakAt ('@':rest) = Just rest
      breakAt (c:rest) = breakAt rest

----------------------------------------------------------------
-- ObjectVersion and ObjectSource
----------------------------------------------------------------

type ObjectVersion = CVSVersion 
-- Read and Show also inherited.  This is made
-- deliberately concise.

data ObjectSource = 
      FileObject String
   |  StringObject String

exportString :: ObjectSource -> IO String
exportString (StringObject str) = return str
exportString (FileObject name) =
   do
      handle <- openFile name ReadMode
      result <- hGetContents handle
      hClose handle -- close file properly, reading contents in
      return result

foreign import "copy_file" unsafe copyFilePrim :: String -> String -> IO Int
-- "unsafe" means we promise that copyFile won't provoke a garbage 
-- collection while it is running.  As copyFile does not call back to
-- Haskell at all, that is guaranteed.

copyFile :: String -> String -> IO ()
copyFile source destination =
   if source == destination 
      then
         return ()
      else
         do
            code <- copyFilePrim source destination
            if (code<0)
               then
                  ioError(userError("CVSDB: Can't copy "++source++" to "++
                     destination++" with error "++show code))
               else
                  return ()

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile (FileObject source) destination = 
   copyFile source destination
exportFile (StringObject str) destination =
   do
      handle <- openFile destination WriteMode
      hPutStr handle str
      hClose handle

importString :: String -> IO ObjectSource
importString str = return (StringObject str)

importFile :: FilePath -> IO ObjectSource
importFile file = return (FileObject file)


Location,
   -- represents Location of object in the repository.  Instance of Read/Show.
   newLocation, -- :: Repository -> ObjectSource -> 
      IO (Location,ObjectVersion)
   -- creates new object in the repository, returning its initial version.
   commit, -- :: Repository -> ObjectSource -> Location -> IO ObjectVersion
   -- commits a new version of the object to the repository, returning its
   -- new version.
   retrieve, -- :: Repository -> Location -> ObjectVersion -> IO ObjectSource
   -- This retrieves an object given the object version to retrieve.

   watch -- :: Repository -> Location -> IA ()
   -- watch returns an event which occurs whenever the object with this
   -- location is touched.
   )


module :: String
module = db
-- all files in the CVS repository lie in this module.




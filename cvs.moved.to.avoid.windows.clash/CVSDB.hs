{-# OPTIONS -#include "copy_file.h" #-}
{- CVSDB is the interface to CVS used by the rest of UniForM.
   The interface to this module is supposed to be standardised,
   meaning that other versioning systems can be substituted.
   -}
module CVSDB(
   Repository,
   RepositoryParameter(WorkingDir),
   -- The workingDir is a directory for the private
   -- use of the CVSDB module.
   initialise, -- :: IO Repository

   ObjectVersion, 
   -- type of versions of objects in the repository
   -- instance of Read/Show/StringClass

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
   -- represents Location of object in the repository.  Instance of 
   -- Read/Show/Eq.
   firstLocation, -- :: Location
   secondLocation, -- :: Location
   -- These two locations are special.  They are already allocated,
   -- and may be used as the user desires.

   newLocation, -- :: Repository -> IO Location
   -- allocate a new unique location in the repository.
   -- and the version of the associated attributes.

   getCVSFilePath, -- :: Repository -> Location -> IO FilePath
   -- get the REAL, complete, file path corresponding to a particular
   -- location.  This allows it to be modified in place.
   -- If this is done, it's important that ObjectSource be
   -- DirectAccess.
   ObjectSource(DirectAccess), 

   commit, -- :: Repository -> ObjectSource -> Location 
      -- -> Maybe ObjectVersion -> IO ObjectVersion
   -- commits a new version of the object to the repository, returning its
   -- new version.  The version, if supplied, should be a previous version of
   -- this object.  If it is NOT supplied, this MUST be the first time
   -- we store to this object.
   retrieveFile, -- :: Repository -> Location -> ObjectVersion -> FilePath ->
                 --       IO ()
   -- retrieveFile retrieves the given version of the object at Location
   -- by copying it to file at FilePath.
   retrieveString, -- :: Repository -> Location -> ObjectVersion -> IO String
   -- retrieveFile retrieves the given version of the object as a String

   listVersions, -- :: Repository -> Location -> IO [ObjectVersion]
   -- listVersion lists all versions of the object with the given location.

   watch, -- :: Repository -> Location -> Event ()
   -- watch returns an event which occurs whenever a commit occurs to the
   -- object with the given location.
   ) where

import IO
import System
import Directory

import PackedString
import CTypesISO(CSize)
import ST

import Debug(debug)
import IOExtras
import WBFiles
import Registry

import Concurrent
import Set
import qualified BSD
import CString
import Storable
import ByteArray(ByteArray)

import Computation(done)
import QuickReadShow
import FiniteMap
import Maybes
import IOExtras
import UniqueFile

import Events


import BSem
import HostsPorts

import Notification
import RegularExpression

import FileNames
import CVSHigh
import LineShow
import AllocateService
import CallServer

import qualified Allocate(firstCVSFile,secondCVSFile)

data Repository = Repository {
   cvsLoc :: CVSLoc, -- CVSHigh location of repository.
   workingDir :: String, 
      -- Working directory.  Includes terminal file separator.
      -- Also includes module name.
   uniqueFileStore :: UniqueFileStore,
      -- Provides place where CVS files are locally stored.
   wDirContents :: LockedRegistry Location ObjectVersion,
      -- wDirContents contains all versions of objects (including
      -- attribute files) currently in the working directory.
   notifier :: Notifier,
   allocator :: AllocateRequest -> IO AllocateAnswer
   -- Calls allocator service
   }

data RepositoryParameter = WorkingDir String 

type Location = CVSFile

----------------------------------------------------------------
-- Initialisation
----------------------------------------------------------------

initialise :: IO Repository
initialise =
   do
      workingDir' <- getWorkingDir

      cvsRootOpt <- getCVSROOT

      cvsRoot <- case cvsRootOpt of
         Just cvsRoot -> return cvsRoot
         Nothing -> getEnv "CVSROOT"

      cvsLoc' <- newCVSLoc cvsRoot workingDir'

      catchAlreadyExists (createDirectory workingDir')
      setCurrentDirectory workingDir'  
 
      cvsCheckoutCheck cvsLoc' (CVSFile cvsModuleName)

      let 
         workingDir = workingDir' ++ [fileSep] ++ cvsModuleName 

      cvsLoc <- newCVSLoc cvsRoot workingDir

      notifier <- mkNotifier

      (allocator,closeAction,header) <- connectReply allocateService

      wDirContents <- newRegistry

      uniqueFileStore <- newUniqueFileStore workingDir
         (\ subDir ->
            do
               createDirectory (combineNames workingDir subDir)
               cvsAdd cvsLoc (CVSFile subDir)
               -- ignore errors, which may be due to directory already
               -- existing in repository
               done
            )
               
      return Repository {
         cvsLoc = cvsLoc,
         workingDir = workingDir,
         wDirContents = wDirContents,
         uniqueFileStore = uniqueFileStore,
         notifier = notifier,
         allocator = allocator
         }
   where
{- This function is now defunct but may prove useful sometime in
   the future . . .

      getHost :: String -> Maybe String
      -- getHost parses a String in CVSROOT format to extract the
      -- host name.  See cvs info page, section "The Repository".
      -- If getHost is Nothing that means the host is unspecified
      -- and so the local one.
      getHost cvsRoot =
         case (breakColons cvsRoot) of
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
            cantParse = error ("Can't parse CVSROOT value: "++cvsRoot)
-}

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

      cvsModuleName :: String
      cvsModuleName = "db" -- name of CVS module containing objects.

----------------------------------------------------------------
-- ObjectVersion and ObjectSource
----------------------------------------------------------------

type ObjectVersion = CVSVersion 
-- Read/Show/StringClass also inherited.  This is made
-- deliberately concise.

data ObjectSource = 
      FileObject String
   |  StringObject String
   |  DirectAccess

exportString :: ObjectSource -> IO String
exportString (StringObject str) = return str
exportString (FileObject name) = readFileInstant name
exportString DirectAccess = directAccessError

foreign import "copy_file" unsafe copyFilePrim 
   :: (ByteArray Int) -> (ByteArray Int) -> IO Int
-- "unsafe" means we promise that copy_file won't provoke a garbage 
-- collection while it is running.  We know this because copy_file
-- doesn't call back to Haskell at all.

copyFile :: String -> String -> IO ()
copyFile source destination =
   if source == destination 
      then
         return ()
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

foreign import "copy_string_to_file" unsafe copyStringToFilePrim 
   :: CSize -> (ByteArray Int) -> (ByteArray Int) -> IO Int

copyStringToFile string destination = writeFile destination string

-- This is a direct encapsulatin of copyStringToFile in C
-- which we turn out not to need, yet.
copyStringToFileAlternative string destination =
   do
      let
         packedString = PackedString.packString string
         packedBytes = psToByteArray packedString
         (sizetLen :: CSize) = fromIntegral (lengthPS packedString)  
         destinationPrim = CString.packString destination 
      code <- copyStringToFilePrim sizetLen packedBytes destinationPrim
      if (code<0)
         then
            ioError(userError("CVSDB: Can't copy string to "++
               destination++" with error "++show code))
         else
            return ()

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile (FileObject source) destination = copyFile source destination
exportFile (StringObject str) destination = copyStringToFile str destination
exportFile DirectAccess _ = directAccessError

exportToCVSFile :: Repository -> ObjectSource -> CVSFile -> IO ()
-- This is only used internally, and exports all sorts of
-- ObjectSource to the CVSFile in question.
exportToCVSFile repository DirectAccess cvsFile = done
exportToCVSFile repository objectSource cvsFile =
   exportFile objectSource (toRealName repository cvsFile)

directAccessError :: IO a
directAccessError =
   error "DirectAccess objects cannot be read from indirectly."
   -- use getCVSFilePath instead

importString :: String -> IO ObjectSource
importString str = return (StringObject str)

importFile :: FilePath -> IO ObjectSource
importFile file = return (FileObject file)

firstLocation :: Location
firstLocation = Allocate.firstCVSFile

secondLocation :: Location
secondLocation = Allocate.secondCVSFile

getCVSFilePath :: Repository -> CVSFile -> IO FilePath
getCVSFilePath repository cvsFile =
   do
      ensureDirs repository cvsFile
      return (toRealName repository cvsFile)
-- getCVSFilePath is identical to toRealName except that
-- it also ensures that the directories are there.

toRealName :: Repository -> CVSFile -> FilePath
toRealName repository (CVSFile location) =
   combineNames (workingDir repository) location

----------------------------------------------------------------
-- Adding an object for the first time
----------------------------------------------------------------


newLocation :: Repository -> IO Location
newLocation repository =
   do
      -- get a new file name
      NewCVSFile cvsFile <- (allocator repository) GetNewCVSFile 
      return cvsFile

initialiseLocation :: Repository -> ObjectSource -> CVSFile ->
   IO ObjectVersion
initialiseLocation (repository@Repository{cvsLoc=cvsLoc}) 
      objectSource cvsFile =
   do
      -- now create object
      ensureDirs repository cvsFile
      -- now add it to repository
      -- object part
      exportToCVSFile repository objectSource cvsFile
      cvsAddCheck cvsLoc cvsFile
      objectVersion <- updateDirContents repository cvsFile
         (\ Nothing -> 
            do
               objectVersionOpt <- cvsCommitCheck cvsLoc cvsFile Nothing
               case objectVersionOpt of
                  Just objectVersion -> return objectVersion
                  -- Nothing indicates cvs claims an existing identical
                  -- version, even though this should be the first one.
            )
         -- a match failure here means someone else in this process
         -- is accessing the file 
      return objectVersion 

----------------------------------------------------------------
-- commit and retrieveFile/retrieveString
----------------------------------------------------------------

commit :: Repository -> ObjectSource -> Location -> Maybe ObjectVersion -> 
   IO ObjectVersion
commit (repository@Repository{cvsLoc=cvsLoc,notifier=notifier}) 
      objectSource (cvsFile@(CVSFile cvsFileName)) parentVersion' =
-- CVS requires that the parent version be in place before we commit.
-- Hopefully this is normally true anyway.
-- Three versions are involved:
-- (1) parentVersion - previous version of file being modified.
--     We need this for CVS.
-- (2) newVersion.   This is the eventual new version of the file.
-- (3) commitVersion.  This is the version to pass to cvsCommit.
--     It is almost the same as newVersion except that when we
--     branch we don't put the final ".1" in. 
-- Example 1.  If we commit a new version to parentVersion 1.1.
-- and no-one else has, we will have newVersion=commitVersion = 1.2.
-- Example 2.  If then we want another new version to parentVersion 1.1,
-- we will have newVersion = 1.1.1.1 and commitVersion = 1.1.1.
   case parentVersion' of
      Nothing -> initialiseLocation repository objectSource cvsFile
      Just parentVersion ->
         do
            -- get the new version number
            NewCVSVersion commitVersion <- 
               (allocator repository) (GetNewCVSVersion cvsFile parentVersion)
            newVersion <- retrieveGeneral repository cvsFile parentVersion 
               (do
                  exportToCVSFile repository objectSource cvsFile
                  newVersionOpt <- 
                     cvsCommitCheck cvsLoc cvsFile (Just commitVersion)
                  case newVersionOpt of
                     Nothing ->
                        -- actually no change here
                        return parentVersion
                     Just newVersion ->
                        do
                           notify notifier cvsFileName
                           return newVersion
                  )
            updateDirContents repository cvsFile
               (\ _ -> return newVersion)
            return newVersion

retrieveFile :: Repository -> Location -> ObjectVersion -> FilePath -> 
   IO ()
retrieveFile repository cvsFile version destination =
   retrieveGeneral repository cvsFile version
      (copyFile (toRealName repository cvsFile) destination)

retrieveString :: Repository -> Location -> ObjectVersion -> IO String
retrieveString repository cvsFile version =
   do
      resultHere <- newEmptyMVar
      retrieveGeneral repository cvsFile version
         (do
            contents <- readFileInstant (toRealName repository cvsFile)
            putMVar resultHere contents
            )
      takeMVar resultHere
      
retrieveGeneral :: Repository -> Location -> ObjectVersion -> 
   (IO result) -> IO result
-- retrieveGeneral repository cvsFile version action
-- ensures that the supplied action is executed when the version of the
-- given file is present.
retrieveGeneral (repository@Repository{cvsLoc=cvsLoc}) cvsFile version 
      action =
   do
      (_,result) <- updateDirContentsGeneral repository cvsFile
         (\ oldVersionOpt ->
            do
               let
                  getFile = cvsUpdateCheck cvsLoc cvsFile version   
               case oldVersionOpt of
                  Just version' | version' == version -> done
                  Just _ -> getFile
                  Nothing -> 
                     do 
                        ensureDirs repository cvsFile
                        getFile
               result <- action
               return (version,result)
            ) 
      return result

         
----------------------------------------------------------------
-- ensureDirs
----------------------------------------------------------------

ensureDirs :: Repository -> CVSFile -> IO ()
ensureDirs repository (CVSFile fileName) =
   ensureDirectories (uniqueFileStore repository) fileName

----------------------------------------------------------------
-- wDirContents 
----------------------------------------------------------------

updateDirContentsGeneral :: Repository -> Location -> 
   (Maybe ObjectVersion -> IO (ObjectVersion,extra)) -> 
   IO (ObjectVersion,extra)
updateDirContentsGeneral (Repository{wDirContents=wDirContents}) location
   transformer =
   transformValue wDirContents location 
      (\ valInOpt ->
         do
            (valOut,extra) <- transformer valInOpt
            return (Just valOut,(valOut,extra))
         )

updateDirContents :: Repository -> Location ->
   (Maybe ObjectVersion -> IO ObjectVersion) -> IO ObjectVersion
updateDirContents repository location actionFn =
   do
      (toReturn,()) <-
         updateDirContentsGeneral repository location
            (\ maybeVersion -> 
               do
                  newVersion <- actionFn maybeVersion
                  return (newVersion,())
               )
      return toReturn


----------------------------------------------------------------
-- Listing versions
----------------------------------------------------------------

listVersions :: Repository -> Location -> IO [ObjectVersion]
listVersions(repository@Repository{cvsLoc=cvsLoc}) cvsFile =
   cvsListVersionsCheck cvsLoc cvsFile

----------------------------------------------------------------
-- Watching files (Of course the commit function also has to use
-- notify for this to work).
----------------------------------------------------------------

watch :: Repository -> Location -> Event ()
watch (repository@Repository{notifier=notifier}) 
      (cvsFile@(CVSFile cvsFileName)) =
   isNotified notifier cvsFileName

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
   -- type of versions of objects in the repository
   -- instance of Read and Show

   AttributeVersion,
   -- type of versions of attributes in the repository.
   -- (Each object has an associated set of attributes, which
   -- are separately versioned)

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
   newLocation, -- :: Repository -> ObjectSource -> 
                --   IO (Location,ObjectVersion,AttributeVersion)
   -- creates new object in the repository, returning its initial version
   -- and the version of the associated attributes.
   newInitialLocation, -- :: Repository -> ObjectSource -> IO ()
   -- creates an object with a new initial location, or else
   -- raise an error.  (Used when file system is initialised)

   commit, -- :: Repository -> ObjectSource -> Location -> IO ObjectVersion
   -- commits a new version of the object to the repository, returning its
   -- new version.
   retrieveFile, -- :: Repository -> Location -> ObjectVersion -> FilePath ->
                 --       IO ()
   -- retrieveFile retrieves the given version of the object at Location
   -- by copying it to file at FilePath.
   retrieveString, -- :: Repository -> Location -> ObjectVersion -> IO String
   -- retrieveFile retrieves the given version of the object as a String

   Attributes, 
   -- type of attribute information.  Attributes are keyed
   -- by a String and may be anything that is an instance of Read and Show  
   getAttribute, -- :: Read a => Attributes -> String -> Maybe a
   setAttribute, -- :: Show a => Attributes -> String -> Maybe a -> Attributes
   getAttributeKeys, -- :: Attributes -> [String]
   emptyAttributes, -- :: Attributes

   -- Attribute information is supposed to be stored in a special
   -- kind of file with a name indicated by the location.
   -- (Yes, this is a hack)
   attLocation, -- :: Location -> Location
   -- get attribute location corresponding to a given location.

   listVersions, -- :: Repository -> Location -> IO [ObjectVersion]
   -- listVersion lists all versions of the object with the given location.

   watch, -- :: Repository -> Location -> IA ()
   -- watch returns an event which occurs whenever a commit occurs to the
   -- object with the given location.

   initialLocation -- :: Location
   -- location we always start with.

   ) where

import IO
import System
import Directory

import Concurrent
import Set
import qualified BSD
import PackedString
import Addr(Addr)

import Computation(done)
import FiniteMap
import Maybes

import Event
import FileEV
import SocketEV

import ExternalEvent(IA)
import Notification
import RegularExpression

import FileNames
import CVSHigh
import LineShow

data Repository = Repository {
   cvsLoc :: CVSLoc, -- CVSHigh location of repository.
   workingDir :: String, 
      -- Working directory.  Includes terminal file separator.
      -- Also includes module name.
   wDirContents :: MVar(FiniteMap Location (MVar(Maybe ObjectVersion))),
      -- wDirContents contains all versions of objects (including
      -- attribute files) currently in the working directory.
   wDirDirs :: MVar (Set Location),
      -- wDirDirs contains the set of all directories known to
      -- already exist in the working directory.
   notifier :: Notifier,
   newINodes :: HandleEV 
      -- HandleEV is the connection to the newInodeServer, see
      -- cvs/inodeserver
   }

data RepositoryParameter =
      HostString String 
      -- For CVS, the CVSROOT parameter.  We also need to contact the
      -- notification server and new inode server; these are 
      -- assumed to exist on the same
      -- machine as the CVS server (for :pserver:) or on this machine,
      -- if CVSROOT is not :pserver:.
   |  WorkingDir String
      -- Does NOT include module name.
      
----------------------------------------------------------------
-- Initialisation
----------------------------------------------------------------

initialise :: [RepositoryParameter] -> IO Repository
initialise options =
   do      
      workingDir' <-
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

      cvsLoc' <- newCVSLoc hostString workingDir'

      hostName <- case getHost hostString of
         Just hostName -> return hostName
         Nothing -> BSD.getHostName

      dirExists <- doesDirectoryExist workingDir'
      if dirExists 
         then
            done
         else
            createDirectory workingDir'
      setCurrentDirectory workingDir'  
 
      cvsCheckoutCheck cvsLoc' (CVSFile cvsModuleName)

      let 
         workingDir = workingDir' ++ [fileSep] ++ cvsModuleName ++ 
            [fileSep]

      cvsLoc <- newCVSLoc hostString workingDir

      notifier <- mkNotifier hostName
 
      inodeserver <- connect hostName (11394::Int)
  
      wDirContents <- newMVar emptyFM
      wDirDirs <- newMVar emptySet

      return Repository {
         cvsLoc = cvsLoc,
         workingDir = workingDir,
         wDirContents = wDirContents,
         wDirDirs = wDirDirs,
         notifier = notifier,
         newINodes = inodeserver
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

      cvsModuleName :: String
      cvsModuleName = "db" -- name of CVS module containing objects.

----------------------------------------------------------------
-- ObjectVersion/AttributeVersion and ObjectSource
----------------------------------------------------------------

type ObjectVersion = CVSVersion 
-- Read and Show also inherited.  This is made
-- deliberately concise.

type AttributeVersion = CVSVersion
-- ditto

data ObjectSource = 
      FileObject String
   |  StringObject String

exportString :: ObjectSource -> IO String
exportString (StringObject str) = return str
exportString (FileObject name) = readFile name

foreign import "copy_file" unsafe copyFilePrim :: Addr -> Addr -> 
   IO Int
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
               sourcePrim = psToCString(packString source)
               destinationPrim = psToCString(packString destination)
            code <- copyFilePrim sourcePrim destinationPrim
            if (code<0)
               then
                  ioError(userError("CVSDB: Can't copy "++source++" to "++
                     destination++" with error "++show code))
               else
                  return ()

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile (FileObject source) destination = 
   copyFile source destination
exportFile (StringObject str) destination = writeFile destination str


importString :: String -> IO ObjectSource
importString str = return (StringObject str)

importFile :: FilePath -> IO ObjectSource
importFile file = return (FileObject file)

type Location = CVSFile
initialCVSFileName :: String
initialCVSFileName = "a"

initialLocation :: Location
initialLocation = CVSFile initialCVSFileName
-- This is derived from inodeserver/NewINodes.hs

toRealName :: Repository -> CVSFile -> FilePath
toRealName repository (CVSFile location) =
   (workingDir repository ++ location)

#ifdef DEBUG
attLocation :: CVSFile -> CVSFile
attLocation (CVSFile str) = CVSFile (postFix str)
   where
      postFix "" = "#"
      postFix (h:t)
         | (h=='#') = error ("CVSDB.attLocation applied to "++str)
         | True = h:postFix t   
#else
attLocation :: CVSFile -> CVSFile
attLocation (CVSFile str) = CVSFile (str++"#")
#endif

----------------------------------------------------------------
-- newLocation and newInitialLocation
----------------------------------------------------------------

newLocation :: Repository -> ObjectSource ->  
   IO (Location,ObjectVersion,AttributeVersion)
newLocation (repository@Repository{newINodes=newINodes}) objectSource =
   do
      -- get a new file name
      writeFileEV newINodes "" -- \n is automatically appended
      cvsFileName <- sync(readFileEV newINodes)
      newGeneralLocation repository objectSource cvsFileName

newInitialLocation :: Repository -> ObjectSource -> IO ()
newInitialLocation (repository@Repository{newINodes=newINodes}) objectSource =
   do
      -- get a new file name
      writeFileEV newINodes "" -- \n is automatically appended
      cvsFileName <- sync(readFileEV newINodes)
      if(initialCVSFileName /= cvsFileName) 
         then
            ioError(userError 
                "Attempt to initialise already-initialised database"
                )
         else
            newGeneralLocation repository objectSource cvsFileName
      done

newGeneralLocation :: Repository -> ObjectSource -> String ->
   IO (Location,ObjectVersion,AttributeVersion)
newGeneralLocation (repository@Repository{cvsLoc=cvsLoc}) 
      objectSource cvsFileName =
   do
      let cvsFile = CVSFile cvsFileName
      -- now create object
      ensureDirectories repository cvsFile
      -- now add it to repository
      -- object part
      exportFile objectSource (toRealName repository cvsFile)
      cvsAddCheck cvsLoc cvsFile
      objectVersion <- updateDirContents repository cvsFile
         (\ Nothing -> cvsCommitCheck cvsLoc cvsFile Nothing 
            )
         -- a match failure here means someone else in this process
         -- is accessing the file    
      -- attribute part  
      let cvsFileAtt = attLocation cvsFile
      writeFile (toRealName repository cvsFileAtt) (show emptyAttributes)
      cvsAddCheck cvsLoc cvsFileAtt
      attributeVersion <- updateDirContents repository cvsFileAtt
         (\ Nothing -> cvsCommitCheck cvsLoc cvsFileAtt Nothing 
            )
         -- a match failure here means someone else in this process
         -- is accessing the file      
      return (cvsFile,objectVersion,attributeVersion)


----------------------------------------------------------------
-- commit and retrieveFile/retrieveString
----------------------------------------------------------------

commit :: Repository -> ObjectSource -> Location -> IO ObjectVersion
commit (repository@Repository{cvsLoc=cvsLoc,notifier=notifier}) 
      objectSource (cvsFile@(CVSFile cvsFileName)) =
   do
      ensureDirectories repository cvsFile
      exportFile objectSource (toRealName repository cvsFile)
      version <- updateDirContents repository cvsFile
         (\ _ -> cvsCommitCheck cvsLoc cvsFile Nothing)
      notify notifier cvsFileName
      return version

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
            contents <- readFile (toRealName repository cvsFile)
            putMVar resultHere contents
            )
      takeMVar resultHere
      
retrieveGeneral :: Repository -> Location -> ObjectVersion -> (IO()) -> IO ()
-- retrieveGeneral repository cvsFile version action
-- ensures that the supplied action is executed when the version of the
-- given file is present.
retrieveGeneral (repository@Repository{cvsLoc=cvsLoc}) cvsFile version 
      action =
   do
      updateDirContents repository cvsFile
         (\ oldVersionOpt ->
            do
               let
                  getFile = cvsUpdateCheck cvsLoc cvsFile version   
               case oldVersionOpt of
                  Just version' | version' == version -> done
                  Just _ -> getFile
                  Nothing -> 
                     do 
                        ensureDirectories repository cvsFile
                        getFile
               action
               return version
            ) 
      done      

         
----------------------------------------------------------------
-- Attributes
----------------------------------------------------------------

newtype Attributes = Attributes(LineShow (String,String))

emptyAttributes :: Attributes
emptyAttributes = Attributes(LineShow [])

getAttribute :: Read a => Attributes -> String -> Maybe a
getAttribute (Attributes(LineShow list)) key =
      searchAttribute list
   where
      searchAttribute [] = Nothing
      searchAttribute ((key',val'):tail) 
         | (key' == key) = Just(read val')
         | True = searchAttribute tail

setAttribute :: Show a => Attributes -> String -> Maybe a -> Attributes
setAttribute (attributes@(Attributes(LineShow list))) key valopt =
   case (split [] list,valopt) of
      (Nothing,Nothing) -> attributes
      (Just (before,after),Nothing) -> al(before++after)
      (Nothing,Just val) -> al((key,show val):list)
      (Just (before,after),Just val) -> 
         al((key,show val):(before++after))
   where
      split acc [] = Nothing
      split acc ((kv@(key',val')):rest)
         | (key' == key) = Just(acc,rest)
         | True = split (kv:acc) rest
      al = Attributes . LineShow
                 
getAttributeKeys :: Attributes -> [String]
getAttributeKeys (Attributes(LineShow list)) =
   map (\ (key,_) -> key) list

instance Show Attributes where
   showsPrec prec (Attributes list) acc = showsPrec prec list acc
 
instance Read Attributes where
   readsPrec prec toRead =   
      let
         result :: [(LineShow (String,String),String)] = readsPrec prec toRead
      in
         map
           (\ (res,rest) -> (Attributes res,rest))
           result

----------------------------------------------------------------
-- ensureDirectories
----------------------------------------------------------------

ensureDirectories :: Repository -> CVSFile -> IO ()
-- ensureDirectories repository file
-- makes sure that all the directories in which file is contained
-- exist, adding it to the repository if necessary.
-- (We assume that adding directories to a repository which
-- are already there is harmless.)
-- A surprisingly complicated function.  Can it be simplified?
-- It needs to be done before every read or write to the archive.
-- Hence we cache the known directories in wDirDirs.
ensureDirectories
      (repository@
         Repository{cvsLoc=cvsLoc,wDirDirs=wDirDirs})
      (CVSFile cvsFile) =
   case splitName cvsFile of
      Nothing -> return () -- no more subdirectories
      Just (dir,_) ->
         do
            let dirCVS = CVSFile dir

            knownDirs <- takeMVar wDirDirs
            if elementOf dirCVS knownDirs
               then
                  do -- nothing
                     putMVar wDirDirs knownDirs
               else
                  do
                     exists <- doesDirectoryExist 
                        (toRealName repository dirCVS)
                     if exists
                        then -- hardly anything
                           do
                              putMVar wDirDirs (addElement dirCVS knownDirs)
                        else -- recurse and (probably) create
                           do
                              putMVar wDirDirs knownDirs
                              ensureDirectories repository dirCVS
                              knownDirs <- takeMVar wDirDirs
                              if elementOf dirCVS knownDirs 
                                 then -- interesting race condition
                                    do
                                       putMVar wDirDirs knownDirs
                                 else
                                    do
                                       createDirectory 
                                          (toRealName repository dirCVS)
                                       cvsAddCheck cvsLoc dirCVS
                                       putMVar wDirDirs 
                                          (addElement dirCVS knownDirs)
   where
      addElement :: Ord a => a -> Set a -> Set a
      addElement newEl set = union set (unitSet newEl)

----------------------------------------------------------------
-- wDirContents 
----------------------------------------------------------------


updateDirContents :: Repository -> Location -> 
   (Maybe ObjectVersion -> IO ObjectVersion) ->  IO ObjectVersion
-- updateDirContents repository location actionFn 
-- is used for all accesses to wDirContents in the 
-- repository.  It updates the version of location according to
-- action.  (actionFn Nothing means we don't already know about this
-- file; actionFn (Just version) means we do and version is its version.
-- It locks so that if updateDirContents is called concurrently on the
-- same file, the actions will not overlap.
updateDirContents (Repository{wDirContents=wDirContents}) 
      location actionFn =
   do
      -- (1) obtain (creating if necessary) the unique MVar corresponding
      --     to this location (and locking it)
      map <- takeMVar wDirContents
      (newMap,mVar) <- case lookupFM map location of
         Nothing -> -- create
            do
               mVar <- newMVar Nothing
               return (addToFM map location mVar,mVar)
         Just mVar -> return (map,mVar)
      putMVar wDirContents newMap
      -- (2) read mVar
      oldVersionOpt <- takeMVar mVar
      -- (3) action
      newVersion <- actionFn oldVersionOpt
      -- (4) put mVar
      putMVar mVar (Just newVersion)
      
      return newVersion

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

watch :: Repository -> Location -> IA ()
watch (repository@Repository{notifier=notifier}) 
      (cvsFile@(CVSFile cvsFileName)) =
   isNotified notifier cvsFileName




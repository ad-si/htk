{- FileSys contains the code which turns the low-level inodes
   into globally versioned files. -}
module FileSys(
   makeFileSys, 
      -- :: [RepositoryParameter] -> IO ()
      -- Set up a repository for the very first time on the remote server
      -- This includes the first "version" which is totally empty.
   FileSys, -- the type of a single File System.
   connectFileSys,
      -- :: [RepositoryParameter] -> IO FileSys
      -- Establish a connection to a FileSys
   Version, -- Type of a version of the file system
   FileObj, -- Object in the file system.  
   FolderObj, -- A folder in the file system

   getVersions, 
      -- :: FileSys -> IO [Version]
      -- Get all versions
   getTop,
      -- :: FileSys -> Version -> IO FileObj
      -- getVersion gets the top of the tree for a particular
      -- version
   getFolderObj,
      -- :: FileSys -> FileObj -> IO FolderObj
      -- This turns a file into the corresponding folder object
   getFolderContents,
      -- :: FileSys -> FolderObj -> [((String,UniType),FileObj)]
   lookupInFolder,
      -- :: FileSys -> FolderObj -> String -> UniType -> Maybe FileObj
   extractFileObj
      -- :: FileSys -> FileObj -> FilePath -> IO ()
      -- copies a whole FileObj into the specified location.
      -- (If a directory, the location should not end with the file
      -- separator)
   
   Change(..),
      -- Change encodes the type of changes in a version
   BrokenPath, 
      -- used in Change
   commitVersion,
      -- :: FileSys -> Version -> FilePath -> [Change] -> 
      --       IO Version
      -- commitVersion makes a new version from the old one
      -- using the changes supplied.  Where new file contents
      -- are included, they are taken from the FilePath.  
      -- In the list of changes, the changes are applied in
      -- the order of the list.
   ) where

import Directory

import IOExts
import FiniteMap
import Concurrent

import Computation
import LineShow
import Cache
import FileNames
import Debug(debug)

import VersionDB
import UniTypes

------------------------------------------------------------------
-- Types
------------------------------------------------------------------

type BrokenPath = [String]
-- Our own local format for local file names.
-- The top directory comes first.  Thus instead of
-- "a/b/c/d" use ["a","b","c","d"].  The list should always be
-- non-empty, as produced by FileNames.breakName

data Change =
-- File names are local to the FileSys.  For NewFile and
-- EditFile, the file contents should be available in
-- the corresponding position relative to FilePath
-- directory (supplied to commitVersion). 
      NewFile BrokenPath -- File (not folder) with this name has been added
   |  NewFolder BrokenPath -- New folder has been added.
   |  EditFile BrokenPath -- File (already in FileSys) has been edited.
   |  RMObject BrokenPath -- remove this file or folder
   |  MVObject BrokenPath BrokenPath -- Move first object to second.

newtype Version = Version ObjectVersion 
-- inherited from VersionDB.  But Version is only
-- used for the top object
data FileObj = FileObj Location ObjectVersion UniType 

instance Eq FileObj where
   (==) (FileObj l1 o1 _) (FileObj l2 o2 _) = (l1,o1) == (l2,o2)
   (/=) (FileObj l1 o1 _) (FileObj l2 o2 _) = (l1,o1) /= (l2,o2)

instance Ord FileObj where
   compare (FileObj l1 o1 _) (FileObj l2 o2 _) = compare (l1,o1) (l2,o2)
   (<=)    (FileObj l1 o1 _) (FileObj l2 o2 _) = (<=)    (l1,o1) (l2,o2)
   (>=)    (FileObj l1 o1 _) (FileObj l2 o2 _) = (>=)    (l1,o1) (l2,o2)
   (<)     (FileObj l1 o1 _) (FileObj l2 o2 _) = (<)     (l1,o1) (l2,o2)
   (>)     (FileObj l1 o1 _) (FileObj l2 o2 _) = (>)     (l1,o1) (l2,o2)

data FolderObj = FolderObj (FiniteMap (String,UniType) FileObj)

data FileSys = FileSys {
   folderCache :: Cache FileObj FolderObj,
   repository :: Repository,
   typeDataBase :: UniTypeDataBase,
   folderType :: UniType
   }

------------------------------------------------------------------
-- Reading and writing folders, and the folder type
------------------------------------------------------------------

readFolderObj :: UniTypeDataBase -> String -> IO FolderObj
readFolderObj typeDataBase str =
   do
      let
         LineShow (contents :: [(String,String,Location,ObjectVersion)]) =  
            read str
      contentsList <- 
         mapM
           (\ (baseName,extension,location,objectVersion) ->
              do
                 uniType <- lookupByExtension typeDataBase extension
                 return (
                    (baseName,uniType),
                    FileObj location objectVersion uniType
                    )
              )
           contents
      let 
         map :: FiniteMap (String,UniType) FileObj = listToFM contentsList
      return(FolderObj map)

writeFolderObj :: FolderObj -> IO ObjectSource
writeFolderObj (FolderObj folderMap) =
   do
      let
         contentsList :: [((String,UniType),FileObj)] =
            fmToList folderMap
         contents :: [(String,String,Location,ObjectVersion)] =
            map
               (\ ((baseName,uniType),FileObj location objectVersion _) ->
                  (baseName,getExtension uniType,location,objectVersion)
                  )
               contentsList
         str = show(LineShow contents)
   
      importString str

-----------------------------------------------------------------
-- Initialisation
------------------------------------------------------------------

makeFileSys :: [RepositoryParameter] -> IO ()
makeFileSys repositoryParameters =
   do
      rep <- initialise repositoryParameters
      initialTopDirectory <-
         writeFolderObj (FolderObj emptyFM)
      newInitialLocation rep initialTopDirectory 
         -- this raises an error if the initial location
         -- has already been allocated
      
connectFileSys :: [RepositoryParameter] -> IO FileSys
connectFileSys repositoryParameters =
   do
      repository <- initialise repositoryParameters
      typeDataBase <- newUniTypeDataBase
      folderCache <-
         newCache
            (\ (FileObj location objectVersion _ ) ->
               do
                  folderStr <- 
                     retrieveString repository location objectVersion
                  readFolderObj typeDataBase folderStr
               ) 

      folderType <- 
         registerType typeDataBase (UniTypeData{name="Folder",extension=""})
      return (FileSys{
         repository=repository,
         folderCache=folderCache,
         typeDataBase=typeDataBase,
         folderType=folderType
         })

------------------------------------------------------------------
-- Reading the file system
------------------------------------------------------------------

getVersions :: FileSys -> IO [Version]
getVersions (fileSys@FileSys{repository=repository}) =
   do 
      objectVersions <- listVersions repository initialLocation 
      return (map 
         (\ objectVersion -> Version objectVersion) 
         objectVersions
         )

getTop :: FileSys -> Version -> IO FileObj
getTop (FileSys{folderType=folderType}) (Version objectVersion) = 
   return (FileObj initialLocation objectVersion folderType)

getFolderObj :: FileSys -> FileObj -> IO FolderObj
getFolderObj 
      (fileSys@FileSys{folderCache=folderCache,folderType=folderType})
      (fileObj@(FileObj _ _ objectType)) =
   do
      if objectType /= folderType
         then
            fileSysError("getFolderObj: not folder type "++
               (show objectType)
               )
         else
            done
      getCached folderCache fileObj
      
getFolderContents :: FileSys -> FolderObj -> [((String,UniType),FileObj)]
getFolderContents _ (FolderObj folderMap) = (fmToList folderMap)

lookupInFolder :: FileSys -> FolderObj -> String -> UniType -> Maybe FileObj
lookupInFolder _ (FolderObj folderMap) baseName uniType =
   lookupFM folderMap (baseName,uniType)

extractFileObj :: FileSys -> FileObj -> FilePath -> IO ()
extractFileObj 
      (fileSys@(FileSys{repository=repository,folderType=folderType}))
      (fileObj@(FileObj location objectVersion uniType)) 
      filePath =
   if uniType /= folderType
      then 
         -- simple case
         retrieveFile repository location objectVersion filePath
      else
         do
            exists <- doesDirectoryExist filePath
            if exists
               then
                  done
               else
                  createDirectory filePath
            folderObj <- getFolderObj fileSys fileObj
            let 
               folderContents = getFolderContents fileSys folderObj
            sequence_
               (map
                  (\ ((baseName,uniType),fileObj) ->
                     do
                        let 
                           partialName = makeFileName baseName uniType
                           completeName = combineNames filePath partialName
                        extractFileObj fileSys fileObj completeName 
                     )
                  folderContents
                  )
------------------------------------------------------------------
-- Updating the file system
------------------------------------------------------------------

commitVersion :: FileSys -> Version -> FilePath -> [Change] -> 
    IO Version
commitVersion fileSys (Version objectVersion) filePath changes =
   do
      -- we first encode the changes in a change tree, also checking for
      -- errors.  Then we commit them.
      changeTree <- encodeAndCheck fileSys objectVersion filePath changes
      newObjectVersion <- commitTree fileSys objectVersion filePath changeTree
      return (Version newObjectVersion)

encodeAndCheck :: FileSys ObjectVersion FilePath Changes -> IO ChangeTree
data ChangeTree = 
      -- This models the folder structure of the file system.
      -- The idea is to contain only the contents of changed folders.
      ExistingObject FileObj
      -- version of file or folder already in database 
   |  EditFile Original FilePath
      -- new version of file, stored at FilePath
   |  EditFolder Original ChangeFolder
      -- new version of folder

newtype ChangeFolder = ChangeFolder (FiniteMap (String,UniType) ChangeTree)

newtype Original = Original (Maybe (Location,ObjectVersion))
-- If the object is new, this is Nothing, otherwise it is
-- Just (the location of the version we are revising)

-- The encode operations apply various update operations on
-- the ChangeTree.  Each one is supplied with arguments:
-- (1) FileSys
-- (2) the FilePath.  This is the (real) path of the folder we are importing.
-- (3) the BrokenPath.  This is the local path name within the folder of
--     the operation to work on.
-- (4) the ChangeTree.  This is the old change tree.
-- They return the new ChangeTree.
-- For getLocation (used for MVObject), we return the (Location,Object)
-- of the location of the BrokenPath.

encodeFileChange :: (Maybe Original) -> FileSys -> FilePath -> ChangeTree -> 
   BrokenPath -> IO ChangeTree
-- This encodes an EditFile operation (add a new file with first argument
-- Nothing, or update a file with first argument the original location).
-- The other arguments are as above.
encodeFileChange oldOriginal fileSys filePath changeTree 
      (brokenPath@(inHere:restPath)) =
   do
      (original,ChangeFolder folderMap) <- getChangeFolder changeTree
      strType@(str,uniType) <- unmakeFileName (typeDataBase fileSys) inHere
      case restPath of
         [] ->
            do
               filePath <- fileExists filePath brokenPath
               assert (uniType /= folderType fileSys) 
                  "encodeFileChange : folderType"
               let
                  newMap = addToFM folderMap strType
                     (EditFile oldOriginal filePath)
               return (EditFolder original (ChangeFolder newMap))
         _ ->
            do
               innerChangeTree <- 
                  case lookupFM folderMap strType of
                     Just innerChangeTree -> return innerChangeTree
                     Nothing -> fileSysError "encodeFileChange A"       
               newInnerTree <- encodeFileChange oldOriginal fileSys 
                     (combineNames filePath inHere) innerChangeTree restPath
               let
                  newTree =
                     EditFolder(original,ChangeFolder(
                        addToFM folderMap strType newInnerTree
                        ))
               return newTree

encodeAndCheck :: FileSys ObjectVersion FilePath Changes -> IO ChangeTree

-- getFolderTree reads a change tree which should correspond to
-- a folder top, and returns the corresponding (Original,ChangeFolder)
-- if it is.
getChangeFolder :: FileSys -> ChangeTree -> IO (Original,ChangeFolder)
getChangeFolder fileSys (EditFile _ _) =
   fileSysError("getChangeError: no folder where one expected")
getChangeFolder fileSys (EditFolder original changeFolder) =
   return (original,changeFolder)
getChangeFolder fileSys (fileObj@(FileObj location objectVersion _)) =
   do 
      FolderObj map <- getFolderObj fileSys fileObj
      let
         changeMap = mapFM
            (\ key -> fileObj -> ExistingObject fileObj)
            map
      return (Original(Just(location,objectVersion)),ChangeFolder changeMap)

fileExists :: FilePath -> BrokenPath -> IO FilePath
-- raises an error if file we are attempting to commit does not exist.
-- Otherwise it returns the complete file name of the resulting file.
fileExists filePath brokenPath =
   do
      let
         completeName = combine (filePath,unbreakName brokenPath)
      exists <- doesFileExist completeName
      if exists
         then
            fileSysError("commitVersion: file "++completeName++
               " does not exist.")
         else
            done

------------------------------------------------------------------
-- Errors
------------------------------------------------------------------

assert :: Bool -> String -> IO ()

#ifdef DEBUG
assert mustBe mess =
   if mustBe 
      then
         done
      else 
         fileSysError mess
#else
assert _ _ = done
#endif

fileSysError :: String -> IO any
fileSysError mess = ioError(userError("FileSys: "++mess++"\n"))


   
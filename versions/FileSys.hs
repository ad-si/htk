{- FileSys contains the code which turns the low-level inodes
   into globally versioned files. -}
module FileSys(
   makeFileSys, 
      -- :: [RepositoryParameter] -> IO ()
      -- Set up a repository for the very first time on the remote server
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
   ) where

import IOExts
import FiniteMap
import Concurrent

import LineShow
import Cache
import Debug(debug)

import VersionDB
import UniTypes

------------------------------------------------------------------
-- Types
------------------------------------------------------------------

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

readFolderObj :: FileSys -> String -> IO FolderObj
readFolderObj (fileSys@(FileSys{typeDataBase=typeDataBase})) str =
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
         map :: FiniteMap String FileObj = listToFM contentsList
      return(FolderObj map)

writeFolderObj :: FolderObj -> ObjectSource
writeFolderObj (FolderObj folderMap) =
   let
      contentsList :: [(String,UniType,FileObj)] =
         fmToList folderMap
      contents :: [(String,String,Location,ObjectVersion)] =
         map
            (\ (baseName,uniType,FileObj location objectVersion _) ->
               (baseName,getExtension uniType,location,objectVersion)
               )
            contentsList
      str = show(LineShow contents)
   in
      StringObject str

-----------------------------------------------------------------
-- Initialisation
------------------------------------------------------------------

makeFileSys :: [RepositoryParameter] -> IO ()
makeFileSys repositoryParameters =
   do
      rep <- initialise repositoryParameters
      let 
         initialTopDirectory :: ObjectSource =
            writeFolderObj (FolderObj emptyFM)
      newInitialLocation rep initialTopDirectory 
         -- this raises an error if the initial location
         -- has already been allocated
      
connectFileSys :: [RepositoryParameter] -> IO FileSys
connectFileSys repositoryParameters =
   do
      repository <- initialise repositoryParameters
      folderCache <-
         newCache
            (\ (FileObj location objectVersion _ ) ->
               do
                  folderStr <- 
                     retrieveString repository location objectVersion
                  readFolderObj folderStr
               ) 

      typeDataBase <- newUniTypeDataBase
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
   listVersions repository initialLocation

getTop :: FileSys -> Version -> IO FileObj
getTop (FileSys{folderType=folderType}) version = 
   return (FileObj initalLocation version folderType)

getFolderObj :: FileSys -> FileObj -> IO FolderObj
getFolderObj 
      (fileSys@FileSys{folderCache=folderCache,folderType=folderType})
      fileObj =
   do
      if objectType /= folderType
         then
            ioError(userError("FileSys.getFolderObj: not folder type "++
               (show objectType)
               ))
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
      (FileSys{repository=repository})
      (FileObj location objectVersion _) 
      filePath =
   retrieveFile repository location objectVersion filePath

   

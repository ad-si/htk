{- FileSys contains the code which turns the low-level inodes
   into globally versioned files.

   Possible Change -- allow file objects to be multiply linked.  The
   only problem with this is that ChangeTree would need an extra field
   (or something like that) and commitTree would need to do some checks
   to make sure that multiply-linked objects are not archived more than 
   once.
    -}
module FileSys(
   makeFileSys, 
      -- :: [RepositoryParameter] -> IO ()
      -- Set up a repository for the very first time on the remote server
      -- This includes the first "version" which is totally empty.
   RepositoryParameter(HostString,WorkingDir), 
      -- taken from CVSDB.
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
   extractFileObj,
      -- :: FileSys -> FileObj -> FilePath -> IO ()
      -- copies a whole FileObj into the specified location.
      -- (If a directory, the location should not end with the file
      -- separator)
   extractFileAttributes,
      -- :: FileSys -> FileObj -> IO Attributes

   -- The following 5 come from CVSDB
   Attributes,
   getAttribute, -- :: Read a => Attributes -> String -> Maybe a
   setAttribute, -- :: Show a => Attributes -> String -> Maybe a -> Attributes
   getAttributeKeys, -- :: Attributes -> [String]
   emptyAttributes, -- :: Attributes
   
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
      -- In the list of changes, the changes are applied with
      -- the first in the list taken first, and so on.
      -- If there are no changes, the version is in fact unchanged.
      -- (That may change . . )

   getTypeDataBase,
      -- :: FileSys -> UniTypeDataBase
      -- This gets the type data base for the file system.
      -- This allows further types to be registered.
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
   |  EditAttributes BrokenPath Attributes -- set attributes for this path.
   |  RMObject BrokenPath -- remove this file or folder
   |  MVObject BrokenPath BrokenPath -- Move first object to second.

newtype Version = Version ObjectVersion 
-- inherited from VersionDB.  But Version is only
-- used for the top object
data FileObj = FileObj Location ObjectVersion AttributeObj UniType 

instance Eq FileObj where
   (==) (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (l1,o1) == (l2,o2)
   (/=) (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (l1,o1) /= (l2,o2)

instance Ord FileObj where
   compare (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = compare (l1,o1) (l2,o2)
   (<=)    (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (<=)    (l1,o1) (l2,o2)
   (>=)    (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (>=)    (l1,o1) (l2,o2)
   (<)     (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (<)     (l1,o1) (l2,o2)
   (>)     (FileObj l1 o1 _ _) (FileObj l2 o2 _ _) = (>)     (l1,o1) (l2,o2)

instance Show FileObj where
   showsPrec prec (FileObj l o a _) acc = showsPrec prec (l,o,a) acc

data FolderObj = FolderObj (FiniteMap (String,UniType) FileObj)

data AttributesObj = AttributesObj Location AttributeVersion
-- only used in the cache

data FileSys = FileSys {
   folderCache :: Cache FileObj FolderObj,
   attributesCache :: Cache AttributesObj Attributes,
   repository :: Repository,
   typeDataBase :: UniTypeDataBase,
   folderType :: UniType
   }

getTypeDataBase :: FileSys -> UniTypeDataBase
getTypeDataBase = typeDataBase

------------------------------------------------------------------
-- Reading and writing folders, and the folder type
------------------------------------------------------------------

readFolderObj :: UniTypeDataBase -> String -> IO FolderObj
readFolderObj typeDataBase str =
   do
      let
         LineShow (contents :: 
               [(String,String,Location,ObjectVersion,AttributeVersion)]) =  
            read str
      contentsList <- 
         mapM
           (\ (baseName,extension,location,objectVersion,attributeVersion) ->
              do
                 uniType <- lookupByExtension typeDataBase extension
                 return (
                    (baseName,uniType),
                    FileObj location objectVersion attributeVersion uniType
                    )
              )
           contents
      let 
         map :: FiniteMap (String,UniType) FileObj = listToFM contentsList
      return(FolderObj map)


writeFolderObj :: FolderObj -> IO ObjectSource
writeFolderObj (FolderObj folderMap) =
   let
      contentsList' :: [((String,UniType),FileObj)] = fmToList folderMap
      contentsList =
         map
            (\ (strType,FileObj location objectVersion attributeVersion _) ->
               (strType,location,objectVersion,attributeVersion)
               )
            contentsList'
   in
      writePrimitiveFolderObj contentsList

writePrimitiveFolderObj :: 
   [((String,UniType),Location,ObjectVersion,AttributeVersion)] -> 
   IO ObjectSource 
writePrimitiveFolderObj contentsList =
   do
      let
         contents :: 
               [(String,String,Location,ObjectVersion,attributeVersion)] =
            map
               (\ ((baseName,uniType),location,objectVersion,
                     attributeVersion) ->
                  (baseName,getExtension uniType,location,objectVersion,
                     attributeVersion)
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

      attributesCache <-
         newCache
            (\ (AttributesObj location attributeVersion) ->
               retrieveAttributes repository location attributeVersion
               )

      folderType <- 
         registerType typeDataBase (UniTypeData{name="Folder",extension=""})
      return (FileSys{
         repository=repository,
         folderCache=folderCache,
         attributesCache=attributesCache,
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
   return (FileObj initialLocation objectVersion initialAttributeVersion
      folderType)

getFolderObj :: FileSys -> FileObj -> IO FolderObj
getFolderObj 
      (fileSys@FileSys{folderCache=folderCache,folderType=folderType})
      (fileObj@(FileObj _ _ _ objectType)) =
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
      (fileObj@(FileObj location objectVersion _ uniType)) 
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

extractFileAttributes :: FileSys -> FileObj -> IO Attributes
extractFileAttributes (FileSys{attributesCache=attributesCache})
   (FileObj location _ attributeVersion _) =
      getCached attributesCache (AttributesObj location attributeVersion)
      

lookupLocalPath :: FileSys -> FileObj -> BrokenPath -> IO FileObj
-- lookupLocalPath looks up an object contained within another following
-- the path names provided.
lookupLocalPath fileSys fileObj [] = return fileObj
lookupLocalPath fileSys fileObj (inHere:rest) =
   do
      folderObj <- getFolderObj fileSys fileObj
      (str,uniType) <- unmakeFileName (typeDataBase fileSys) inHere
      case lookupInFolder fileSys folderObj str uniType of
         Nothing -> fileSysError ("lookupLocalPath : "++inHere++" not found")
         Just fileObj -> lookupLocalPath fileSys fileObj rest

------------------------------------------------------------------
-- Updating the file system
------------------------------------------------------------------

commitVersion :: FileSys -> Version -> FilePath -> [Change] -> 
    IO Version
commitVersion fileSys version filePath changes =
   do
      -- we first encode the changes in a change tree, also checking for
      -- errors.  Then we commit them.
      changeTree <- encodeAndCheck fileSys version filePath changes
      debug changeTree
      newObjectVersion <- commitTree fileSys changeTree
      return (Version newObjectVersion)

data ChangeTree = 
      -- This models the folder structure of the file system.
      -- The idea is to contain only the contents of changed folders.
      NewObject NewContents Attributes
      -- new file at this path with its attributes
   |  OldObject OldContents (Maybe Attributes) FileObj
      -- already existing object with new contents and attributes if set.
   deriving Show

data NewContents = 
      NewFile FilePath
   |  NewFolder ChangeFolder
   deriving Show

data OldContents =
      Unchanged
   |  EditFile FilePath
   |  EditFolder ChangeFolder
   deriving Show

newtype ChangeFolder = ChangeFolder (FiniteMap (String,UniType) ChangeTree)
   
instance Show ChangeFolder where
   showsPrec prec (ChangeFolder map) acc = showsPrec prec (fmToList map) acc

------------------------------------------------------------------
-- Turning sequences of changes into change trees.
------------------------------------------------------------------

encodeAndCheck :: FileSys -> Version -> FilePath -> [Change] -> IO ChangeTree
encodeAndCheck fileSys version filePath changes =
   do
      (top :: FileObj) <- getTop fileSys version
      let
         initialChangeTree = ExistingObject top
         
         encode :: [Change] -> ChangeTree -> IO ChangeTree
         encode [] changeTree = return changeTree
         encode (firstChange:restChanges) changeTree =
            do
               nextChange <- case firstChange of
                  NewFile brokenPath ->
                     encodeFileChange 
                        True filePath fileSys changeTree brokenPath
                  NewFolder brokenPath ->
                     encodeNewFolder fileSys changeTree brokenPath
                  EditFile brokenPath ->
                     encodeFileChange 
                        False filePath fileSys changeTree brokenPath
                  RMObject brokenPath ->
                     encodeRMObject fileSys changeTree brokenPath
                  MVObject brokenPathFrom brokenPathTo ->
                     do 
                        treeOfOld <- 
                           getLocation fileSys brokenPathFrom changeTree
                        withDelete <- encodeRMObject 
                           fileSys changeTree brokenPathFrom
                        encodeInsertion 
                           treeOfOld fileSys withDelete brokenPathTo
               encode restChanges nextChange

      encode changes initialChangeTree

-- All updates to ChangeTrees use the following function for
-- updating propagating changes up through the folders.
-- Input is a function ((String,UniType) -> ChangeFolder -> IO ChangeFolder).
-- Output is a function (FileSys -> ChangeTree -> BrokenPath -> IO ChangeTree)
-- The BrokenPath should have at least one element.  The effect is to
-- apply the supplied function to the ChangeTree element corresponding to
-- the last element of the BrokenPath, and then construct folders
-- containing the resulting new element in its place.
encodeChangeTreeUpdate :: ((String,UniType) -> ChangeFolder -> ChangeFolder) 
   -> (FileSys -> ChangeTree -> BrokenPath -> IO ChangeTree)
encodeChangeTreeUpdate updateFn
      fileSys changeTree (brokenPath@(inHere:restPath)) =
   do
      (changeFolder@(ChangeFolder folderMap),wrapper) <-
         getChangeFolder fileSys changeTree
      strType@(str,uniType) <- unmakeFileName (typeDataBase fileSys) inHere
      case restPath of
         [] ->
            let
               newChangeFolder = updateFn strType changeFolder
            in
               return (wrapper newChangeFolder)
         _ ->
            do 
               assert (uniType == folderType fileSys) 
                  "encodeChangeTreeUpdate: A"
               innerChangeTree <- 
                  case lookupFM folderMap strType of
                     Just innerChangeTree -> return innerChangeTree
                     Nothing -> fileSysError "encodeChangeTreeUpdate B"       
               newInnerTree <- 
                  encodeChangeTreeUpdate
                     updateFn fileSys innerChangeTree restPath
               let
                  newTree =
                     wrapper 
                       (ChangeFolder(
                        addToFM folderMap strType newInnerTree
                        ))
               return newTree
   where
      -- getChangeFolder reads a ChangeTree which should correspond to
      -- a folder top.  It returns (1) a ChangeFolder corresponding to the
      -- contents; (2) a function for wrapping up a new ChangeFolder
      -- in a ChangeTree as a new version of that ChangeFolder. 
      getChangeFolder :: FileSys -> ChangeTree ->
         IO (ChangeFolder,ChangeFolder -> ChangeTree)
      getChangeFolder fileSys 
            (NewObject (NewFolder changeFolder) attributes) =
         return (
            changeFolder,
            \ newChangeFolder ->
               (NewObject (NewFolder newChangeFolder) attributes)
            )
      getChangeFolder fileSys
            (NewObject (NewFile changeFolder) attributes) =
         fileSysError("getChangeError: no folder where one expected")
      getChangeFolder fileSys 
            (OldObject Unchanged maybeAttributes fileObj) =
         do 
            FolderObj map <- getFolderObj fileSys fileObj
            let
               changeFolder = ChangeFolder(mapFM
                  (\ key fileObj -> OldObject Unchanged Nothing fileObj)
                  map
                  )
            return ( 
               changeFolder,
               \ newChangeFolder ->
                  OldObject (EditFolder newChangeFolder) maybeAttributes 
                     fileObj
               )
      getChangeFolder fileSys 
            (OldObject (EditFile _) maybeAttributes fileObj) =
         fileSysError("getChangeError: no folder where one expected (2)")
      getChangeFolder fileSys 
            (OldObject (EditFolder changeFolder) maybeAttributes fileObj) =
         return (
            changeFolder,
            \ newChangeFolder ->
               (OldObject (EditFolder newChangeFolder) maybeAttributes 
                  fileObj)
            )

encodeFileChange :: Bool -> FilePath -> FileSys -> ChangeTree -> 
   BrokenPath -> IO ChangeTree
-- encodeFileChange is an encodeChangeTreeUpdate-style update of a
-- changeTree which is used to insert a new file (if the first argument
-- is True) or a new version of an old one (otherwise).  The location of
-- the file is given by combining FilePath + BrokenPath, and it i
-- checked that it exists.
encodeFileChange isNew filePath fileSys changeTree brokenPath =
   do
      let
         completeName = unbreakName (filePath : brokenPath)
      exists <- doesFileExist completeName
      if not exists
         then
            fileSysError("encodeFileChange: file "++completeName++
               " does not exist.")
         else
            done
      let
         updateFn :: (String,UniType) -> ChangeFolder -> ChangeFolder
         updateFn strType (ChangeFolder folderMap) =
            let
               newChangeTree :: ChangeTree =
                  if isNew
                     then
                        NewObject (NewFile completeName) emptyAttributes
                     else
                        case lookupFM folderMap strType of
                           Just (OldObject Unchanged attributes fileObj) ->
                              (OldObject (EditFile completeName) attributes 
                                 fileObj)
                           Just (OldObject (EditFile _) attributes fileObj) ->
                              (OldObject (EditFile completeName) attributes 
                                 fileObj)
                           -- match error if file is a folder instead.
                           Nothing -> 
                              let
                                 (str,uniType) = strType
                                 fname = makeFileName str uniType
                              in
                                 error (
                                    "FileSys.encodeFileChange: In file edit "
                                    ++completeName++" does not exist") 
               newMap = addToFM folderMap strType newChangeTree
            in
               ChangeFolder new
      encodeChangeTreeUpdate updateFn fileSys changeTree brokenPath

encodeNewFolder :: FileSys -> ChangeTree -> BrokenPath -> IO ChangeTree
-- encodeNewFolder adds a new folder
encodeNewFolder fileSys changeTree brokenPath =
   encodeInsertion (NewObject (NewFolder (ChangeFolder emptyFM)) emptyFolder)
      fileSys changeTree brokenPath

encodeRMObject :: FileSys -> ChangeTree -> BrokenPath -> IO ChangeTree
encodeRMObject fileSys changeTree brokenPath =
-- encodeRMObject removes an object
   let
      updateFn :: (String,UniType) -> ChangeFolder -> ChangeFolder
      updateFn strType (ChangeFolder folderMap) =
         let
            newMap = delFromFM folderMap strType 
         in
            ChangeFolder newMap
   in
      encodeChangeTreeUpdate updateFn fileSys changeTree brokenPath

encodeInsertion :: ChangeTree -> FileSys -> ChangeTree -> 
   BrokenPath -> IO ChangeTree
-- encodeInsertion adds a new (presumably already existing) object with
-- the given location and objectVersion.  Combined with getLocation, that
-- allows us to do relinking.  We also use it for encodeNewFolder.
encodeInsertion changeTreeToInsert fileSys changeTree brokenPath =
   let
      updateFn :: (String,UniType) -> ChangeFolder -> ChangeFolder
      updateFn strType (ChangeFolder folderMap) =
         let
            newMap = addToFM folderMap strType changeTreeToInsert
         in
            ChangeFolder newMap
   in
      encodeChangeTreeUpdate updateFn fileSys changeTree brokenPath

getLocation :: FileSys -> BrokenPath -> ChangeTree -> 
   IO ChangeTree
getLocation fileSys [] changeTree = return changeTree
getLocation fileSys brokenPath (OldObject Unchanged _ fileObj) =
   do
      fileObj <- lookupLocalPath fileSys fileObj brokenPath
      return (OldObject Unchanged Nothing fileObj)
getLocation fileSys brokenPath 
      (OldObject (EditFolder (ChangeFolder map)) _ _) = 
         lookupInFolder brokenPath map
getLocation fileSys brokenPath
      (NewObject (NewFolder (ChangeFolder map)) _) =
         lookupInFolder brokenPath map 
   where
      lookupInFolder (inHere:rest) map =
         do
            strType <- unmakeFileName (typeDataBase fileSys) inHere
            case lookupFM map strType of
               Just changeTree -> getLocation fileSys rest changeTree

------------------------------------------------------------------
-- Committing a ChangeTree
------------------------------------------------------------------

commitTree :: FileSys -> ChangeTree -> IO ObjectVersion
-- commitTree commits a new version returning the new version.
-- (If the tree contains no changes the new version will in fact
-- be the same as the old one.)
commitTree fileSys changeTree =
   do
      (location,objectVersion) <- commitTree' fileSys changeTree
      assert (location==initialLocation)
         "commitTree A"
      return objectVersion

-- commitTree' commits a changeTree and returns its new location and
-- objectVersion.  (For the top of the changeTree the new location
-- had better be the same as the old one of course.)
commitTree' :: FileSys -> ChangeTree -> IO (Location,ObjectVersion)
commitTree' (fileSys@FileSys{repository=repository}) changeTree =
   case changeTree of
      ExistingObject (FileObj location objectVersion _) -> 
         return (location,objectVersion)
      UpdateFile (Original Nothing) filePath ->
         do
            objectSource <- importFile filePath
            (location,objectVersion,_) <- 
               newLocation repository objectSource
            return (location,objectVersion)
      UpdateFile (Original(Just(location,oldObjectVersion))) filePath ->
         do
            objectSource <- importFile filePath
            objectVersion <- 
               commit repository objectSource location oldObjectVersion
            return (location,objectVersion)
      UpdateFolder (Original original) (ChangeFolder changeFolderMap) ->
         do
            let
               folderContents :: [((String,UniType),ChangeTree)] =
                  fmToList changeFolderMap
               folderContents2 :: 
                     [IO ((String,UniType),Location,ObjectVersion)] =
                  map
                     (\ (strType,changeTree) ->
                        do
                           (location,objectVersion) <- 
                              commitTree' fileSys changeTree
                           return (strType,location,objectVersion)
                        ) 
                     folderContents
            (folderContents3 :: [((String,UniType),Location,ObjectVersion)])
               <- sequence folderContents2
            objectSource <- writePrimitiveFolderObj folderContents3
            case original of
               Nothing ->
                  do
                     (location,objectVersion,_) <-
                        newLocation repository objectSource
                     return (location,objectVersion)
               Just (location,oldObjectVersion) ->
                  do
                     objectVersion <- commit repository objectSource location
                        oldObjectVersion
                     return (location,objectVersion)
 
         
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

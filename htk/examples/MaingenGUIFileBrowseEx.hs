-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Main (main) where

import HTk
import GenGUI
import Name
import ReferenceVariables
import IOExts(unsafePerformIO)
import Directory
import System(getEnv)


-- -----------------------------------------------------------------------
-- file types
-- -----------------------------------------------------------------------

type FileType = (String,          -- comment
                 String)          -- image filename

fileTypes :: [([String], FileType)]
fileTypes = [(["hs", "lhs"], ("Haskell Source File", "haskell.gif")),
             (["hi"], ("Haskell Interface File", "haskelli.gif"))]

unknown :: FileType
unknown = ("Unknown", "unknown.gif")

folder :: FileType
folder = ("Folder", "folder.gif")

getFileTypeByExtension :: String -> FileType
getFileTypeByExtension ext =
  let getFileTypeByExtension' ((exts, ft) : tps) =
        if any ((==) ext) exts then ft else getFileTypeByExtension' tps
      getFileTypeByExtension' _ = unknown
  in getFileTypeByExtension' fileTypes


-- -----------------------------------------------------------------------
-- file objects
-- -----------------------------------------------------------------------

type Id = Int

idref = unsafePerformIO (newRef 0)
 
newID :: IO Id
newID = do id <- getRef idref
           setRef idref (id + 1)
           return id

data FileObject = FileObject { fid       :: Id,
                               fname     :: Ref FilePath,
                               fdir      :: Ref FilePath,
                               filetype  :: Ref FileType,
                               icon      :: Ref Image,
                               is_folder :: Bool }

getFileObjectId :: FileObject -> Id
getFileObjectId obj = fid obj

getFileObjectFileName :: FileObject -> IO FilePath
getFileObjectFileName obj = getRef (fname obj)

getFileObjectDirectory :: FileObject -> IO FilePath
getFileObjectDirectory obj = getRef (fdir obj)

getFileObjectFullPath :: FileObject -> IO FilePath
getFileObjectFullPath obj =
  do dir <- getFileObjectDirectory obj
     nm <- getFileObjectFileName obj
     return (dir ++ "/" ++ nm)

getFileObjectName :: FileObject -> IO Name
getFileObjectName obj =
  do nm <- getFileObjectFileName obj
     return (createName nm)

getFileObjectIcon :: FileObject -> IO Image
getFileObjectIcon obj = getRef (icon obj)

getFileObjectFileType :: FileObject -> IO FileType
getFileObjectFileType obj = getRef (filetype obj)

isFileObjectFolder :: FileObject -> Bool
isFileObjectFolder obj = is_folder obj

instance Eq FileObject where
  obj1 == obj2 = fid obj1 == fid obj2

instance CItem FileObject where
  getName obj = getFileObjectName obj
  getIcon obj = getFileObjectIcon obj

getFileType :: FilePath -> FileType
getFileType nm =
  let extension nm = if any ((==) '.') nm then
                       reverse (extension' (reverse nm))
                     else ""
      extension' (c : cs) = if c == '.' then [] else c : extension' cs
      extension' _ = ""
      ext = extension nm
  in getFileTypeByExtension ext

isFolder :: FilePath -> FilePath -> IO Bool
isFolder dir nm =
  do p <- getPermissions (dir ++ "/" ++ nm)
     return (searchable p)

toFileObject :: FilePath -> FilePath -> IO FileObject
toFileObject dir nm =
  do obj_fid <- newID
     obj_fname <- newRef nm
     obj_fdir <- newRef dir
     obj_is_folder <- isFolder dir nm
     let ft@(_, img_path) = if obj_is_folder then folder
                            else getFileType nm
     obj_filetype <- newRef ft
     icon <- newImage NONE [filename ("images/" ++ img_path)]
     obj_icon <- newRef icon
     return (FileObject { fid       = obj_fid,
                          fname     = obj_fname,
                          fdir      = obj_fdir,
                          filetype  = obj_filetype,
                          icon      = obj_icon,
                          is_folder = obj_is_folder })


-- -----------------------------------------------------------------------
-- filesystem functionality
-- -----------------------------------------------------------------------

readDir :: FilePath -> IO [FileObject]
readDir dir =
  do dc <- getDirectoryContents dir
     let dc' = filter (\f -> f /= "." && f /= ".." && not (hidden f)) dc
     mapM (toFileObject dir) dc'

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _ = False


-- -----------------------------------------------------------------------
-- initialize gui
-- -----------------------------------------------------------------------

initGUI :: GenGUI FileObject -> FilePath -> IO ()
initGUI gui dir =
  do root_file_objects <- readDir dir
     rt <- root gui
     let addObjects (obj : objs) =
           (if isFileObjectFolder obj then
              do newitem <- toNewItem obj
                 addItem gui rt newitem >> done
            else done) >>
           addObjects objs
         addObjects _ = done
     addObjects root_file_objects
  where toNewItem :: FileObject -> IO (NewItem FileObject)
        toNewItem obj =
          if isFileObjectFolder obj then
            do path <- getFileObjectFullPath obj
               contents <- readDir path
               contents' <- mapM toNewItem contents
               return (FolderItem obj contents' Nothing)
         else return (LeafItem obj Nothing)


-- -----------------------------------------------------------------------
-- main
-- -----------------------------------------------------------------------

main :: IO ()
main =
  do
    htk <- initHTk [withdrawMainWin]
    (gui :: GenGUI FileObject) <- newGenGUI Nothing False
    initGUI gui "/home/cxl/opt/hdoc-0.8.0"
    (destr, _) <- bindSimple gui Destroy
    spawnEvent (destr >>> destroy htk)
    finishHTk

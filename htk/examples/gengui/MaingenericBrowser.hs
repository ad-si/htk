
module Main(main) where

import Control.Exception
import Directory

import System.IO.Unsafe

import HTk.Toplevel.HTk
import HTk.Toolkit.Name
import HTk.Toolkit.CItem
import Reactor.ReferenceVariables
import HTk.Toolkit.GenericBrowser
import Data.Char(toLower)


-- -----------------------------------------------------------------------
-- file types
-- -----------------------------------------------------------------------

type FileType = (String,          -- comment
                 String)          -- image filename

fileTypes :: [([String], FileType)]
fileTypes = [(["hs", "lhs"], ("Haskell source file", "haskell.gif")),
             (["hi"], ("Haskell Interface File", "haskelli.gif")),
             (["gif"], ("GIF image", "image.gif")),
             (["jpg"], ("JPG image", "image.gif")),
             (["png"], ("PNG image", "image.gif")),
             (["tif", "tiff"], ("TIFF image", "image.gif")),
             (["bmp"], ("BMP image", "image.gif")),
             (["tex"], ("TeX file", "tex.gif")),
             (["bib"], ("BibTeX file", "bib.gif")),
             (["ps"], ("PostScript", "postscript.gif")),
             (["tar"], ("TAR archive", "archive.gif")),
             (["gz"], ("GZ archive", "archive.gif")),
             (["zip"], ("ZIP archive", "archive.gif")),
             (["rar"], ("RAR archive", "archive.gif")),
             (["html", "htm"], ("Hypertext document", "html.gif"))]

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
-- util
-- -----------------------------------------------------------------------

type Id = Int

idref = unsafePerformIO (newRef 0)
{-# NOINLINE idref #-}

newID :: IO Id
newID = do id <- getRef idref
           setRef idref (id + 1)
           return id


-- -----------------------------------------------------------------------
-- filesystem functionality
-- -----------------------------------------------------------------------

readDir :: FilePath -> IO [FileObject]
readDir dir =
  do
     ret <- try (do dc <- getDirectoryContents dir
                    let dc' = filter (\f -> f /= "." && f /= ".." &&
                                      not (hidden f)) dc
                    mapM (toFileObject dir) dc')
     case ret of Right objs -> return objs
                 Left (_ :: SomeException) -> return []

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _ = False

isFolder :: FilePath -> FilePath -> IO Bool
isFolder dir nm = doesDirectoryExist (dir ++ "/" ++ nm)

getFileType :: FilePath -> FileType
getFileType nm =
  let extension nm = if any ((==) '.') nm then
                       reverse (extension' (reverse nm))
                     else ""
      extension' (c : cs) = if c == '.' then [] else c : extension' cs
      extension' _ = ""
      ext = map toLower (extension nm)
  in getFileTypeByExtension ext

toFileObject :: FilePath -> FilePath -> IO FileObject
toFileObject dir nm =
  do obj_fid <- newID
     obj_fname <- newRef nm
     obj_fdir <- newRef dir
     obj_is_folder <- isFolder dir nm
     let ft@(_, img_path) = if obj_is_folder then folder
                            else getFileType nm
     obj_filetype <- newRef ft
     icon <- newImage [filename ("images/" ++ img_path)]
     obj_icon <- newRef icon
     return (FileObject { fid       = obj_fid,
                          fname     = obj_fname,
                          fdir      = obj_fdir,
                          filetype  = obj_filetype,
                          icon      = obj_icon,
                          is_folder = obj_is_folder })


-- -----------------------------------------------------------------------
-- file objects
-- -----------------------------------------------------------------------

data FileObject = FileObject { fid       :: Id,
                               fname     :: Ref FilePath,
                               fdir      :: Ref FilePath,
                               filetype  :: Ref FileType,
                               icon      :: Ref Image,
                               is_folder :: Bool }

instance Eq FileObject where
  obj1 == obj2 = fid obj1 == fid obj2

instance CItem FileObject where
  getName obj = getFileObjectName obj
  getIcon obj = getFileObjectIcon obj

instance GBObject FileObject where
  isObjectNode = return . is_folder
  getChildren obj = do
                       path <- getFileObjectFullPath obj
                       readDir path


-- -----------------------------------------------------------------------
-- selectors
-- -----------------------------------------------------------------------

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


-- -----------------------------------------------------------------------
-- main
-- -----------------------------------------------------------------------

getRootObjects :: IO [FileObject]
getRootObjects = readDir "/"

main :: IO ()
main = do htk <- initHTk [size(800, 500), text "GenericBrowser example"]
          root_objs <- getRootObjects
          (gb :: GenericBrowser FileObject) <-
            newGenericBrowser htk root_objs []
          (action, _) <- bindGenericBrowserEv gb
          spawnEvent (forever (do ev <- action
                                  always (printEv ev)))
          pack gb [Fill Both, Expand On]
          bottom <- newFrame htk []
          quit <- newButton bottom [text "Quit"]
          clicked_quit <- clicked quit
          spawnEvent (clicked_quit >>> destroy htk)
          pack quit [Side AtBottom, Fill X, PadY 5, PadX 50]
          pack bottom [Side AtBottom, Fill X]
          finishHTk

printEv :: GenericBrowserEvent FileObject -> IO ()
printEv ev = case ev of
               SelectedInTreeList Nothing -> putStrLn "no selection (treelist)"
               SelectedInTreeList _ -> putStrLn "selection (treelist)"
               FocusedInTreeList _ -> putStrLn "focus (treelist)"
               Dropped _ -> putStrLn "drop (notepad)"
               SelectedInNotepad _ -> putStrLn "selection (notepad)"
               DeselectedInNotepad _ -> putStrLn "deselection (notepad)"
               Doubleclick _ -> putStrLn "doubleclick (notepad)"
               Rightclick _ -> putStrLn "rightclick (notepad)"
               _ -> done

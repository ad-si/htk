{- ------------------------------------------------------------------------
 -
 - Module FileDialog
 -
 - Simple file dialog
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - ------------------------------------------------------------------------ -}


module FileDialog (

  launchFileDialog

) where

import HTk
import Concurrency
import ListBox
import ScrollBar
import Label
import Button
import Entry
import Image
import MenuButton
import Menu
import MenuItem
import PulldownMenu
import Mouse
import Directory
import RVar

debug = True
debugMsg str = if debug then putStr(">>> " ++ str ++ "\n\n") else done

getFilesAndFolders :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndFolders path =
  do
    dcontents <- getDirectoryContents path
    sort dcontents [] [] path
  where sort :: [FilePath] -> [FilePath] -> [FilePath] -> FilePath ->
                IO ([FilePath], [FilePath])
        sort (f : fs) files folders abs =
          if f == "." || f == ".." then sort fs files folders abs else
            do
              b <- getPermissions (abs ++ "/" ++ f)
              if searchable b then sort fs files (f : folders) abs
                              else sort fs (f : files) folders abs
        sort _ files folders _ =
          return (sort' files, if path == "/" then sort' folders
                                              else ".." : (sort' folders))
        sort' :: [FilePath] -> [FilePath]
        sort' (f : fs) = sort'(filter (\f' -> f' < f) fs) ++ [f] ++
                         sort'(filter (\f' -> f' > f) fs)
        sort' _ = []

dropLast :: FilePath -> FilePath
dropLast path = dropLast' (tail (reverse path))
  where dropLast' :: String -> String
        dropLast' (c : cs) =
          if c == '/' then reverse (c : cs) else dropLast' cs

selectedFolder :: Int -> RVar [FilePath] -> RVar [FilePath] -> RVar FilePath ->
                  ListBox [String] -> ListBox [String] -> IO ()
selectedFolder i foldersref filesref pathref folderslb fileslb =
  do
    folders <- getVar foldersref
    path <- getVar pathref
    nupath <- return (if (folders !! i) == ".." then
                        dropLast path
                      else path ++ (folders !! i) ++ "/")
    b <- getPermissions nupath
    (if readable b then
       do
         putStrLn ("new path is: " ++ nupath)
         setVar pathref nupath
         (files, folders) <- getFilesAndFolders nupath
         setVar filesref files
         setVar foldersref folders
         folderslb # value folders
         fileslb # value files
         done
     else debugMsg "Permission denied")

updPathMenu :: MenuButton String -> Menu String -> RVar FilePath -> RVar [Button String] -> IO ()
updPathMenu menubutton menu pathref menuref = done
{-
  do
    path <- getVar pathref
    menubutton # text path
    menuitems <- getVar menuref
    mapM destroy menuitems
    nuitems <- addPaths menu [] (getUpperPaths path)
    setVar menuref nuitems
  where getUpperPaths :: FilePath -> [FilePath]
        getUpperPaths "/" = []
        getUpperPaths p = p : getUpperPaths (dropLast p)
        addPaths :: Menu String -> [Button String]-> [FilePath] -> IO [Button String]
        addPaths menu items (p : ps) =
          do
            item <- newMenuItem menu [text p]
            addPaths menu (item : items) ps
        addPaths _ items _ = return items
-}

launchFileDialog :: String -> FilePath -> IO String
launchFileDialog str path =
  do
    main <- newVFBox []
    win <- window main [text str]
    (files, folders) <- getFilesAndFolders path
    pathref <- newRVar path
    filesref <- newRVar files
    foldersref <- newRVar folders
    menuref <- newRVar [] :: IO (RVar [Button String])
    actions <- newHFBox [parent main]
    newFolderImg' <- newFolderImg
    upImg' <- upImg
    deleteFileImg' <- deleteFileImg
    up <- newButton [pad Horizontal 10, pad Vertical 5, parent actions,
                     photo upImg']
    newFolder <- newButton [pad Horizontal 10, pad Vertical 5,
                                photo newFolderImg', parent actions]
    deleteFile <- newButton [pad Horizontal 10, pad Vertical 5, parent actions,
                             photo deleteFileImg']
    pathmenubutton <- newMenuButton [pad Horizontal 10, pad Vertical 10, parent main,
                                     text path]
    pathmenu <- newPulldownMenu pathmenubutton [tearOff Off]
    updPathMenu pathmenubutton pathmenu pathref menuref
    boxes <- newHFBox [parent main]
    folderslist <- newHFBox [pad Horizontal 10, pad Vertical 10, parent boxes]
    folderslb <- newListBox [value folders, size (30, 15), bg "white",
                             side AtLeft, parent folderslist]
    foldersscb <- newScrollBar [parent folderslist, side AtRight,
                                fill Vertical]
    folderslb # scrollbar Vertical foldersscb
    fileslist <- newHFBox [pad Horizontal 10, pad Vertical 10, parent boxes]
    fileslb <- newListBox [value files, size (30, 15), bg "white", side AtLeft,
                           parent fileslist]
    filesscb <- newScrollBar [parent fileslist, side AtRight, fill Vertical]
    fileslb # scrollbar Vertical filesscb
    fileEntry <- newEntry [width 60, bg "white", pad Horizontal 10,
                           pad Vertical 10, parent main] :: IO (Entry String)
    buttons <- newHFBox [parent main]
    ok <- newButton [pad Horizontal 10, pad Vertical 5, text "Ok",
                     parent buttons, command (\ () -> destroy win)]
    quit <- newButton [pad Horizontal 10, pad Vertical 5, text "Quit",
                       parent buttons,
                       command (\ () -> fileEntry # value "" >> destroy win)]
    interactor (\i -> triggered ok +> triggered quit +> triggered up +>
                      triggered newFolder +> triggered deleteFile)
    interactor (\i -> (mouseButtonPress folderslb 1 >>>
                         do
                           sel <- getSelection folderslb :: IO (Maybe [Int])
                           case sel of
                             Just (i : _) -> selectedFolder i foldersref
                                               filesref pathref folderslb
                                               fileslb >>
                                             updPathMenu pathmenubutton
                                               pathmenu pathref menuref
                             Nothing -> done))
    sync (destroyed win)
    return "Hallo"

newFolderImg = newImage [imgData GIF "R0lGODdhFAAUAPAAAAAA/wD//ywAAAAAFAAUAMcAAAD//wD///8AAAD///8AAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
XwAFCBxIsKBBAAESKlzIMAAAAAIQNpyY8GFEiwYzDpRIseFDiCA1itTIUSFEgSU7mgSp0uPJix9j
ysQ4cuTHmgVDvtwoM+fMlCZRtmRoEajKokMXIk1a8eTPpzdxZgwIADs="]

upImg = newImage [imgData GIF "R0lGODdhFAAUAPAAAAAA/wD//ywAAAAAFAAUAMcAAAD//wD///8AAAD///8AAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
YAAFCBxIsKBBAAESKlzIMAAAAAIQNpyY8KHBixglUqyo0CLGjyAFauw4cOTEhxItpkTJ8uFCjxFb
ymQZsqbNmyJbFpxpkmTEjR07Quz50qFRlUY3akQKlKFHnlAh4iQYEAA7"]

deleteFileImg = newImage [imgData GIF "R0lGODdhFAAUAPAAAAAA/wD//ywAAAAAFAAUAMf/AAD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
TwADCBxIsKDBggASBkgI4KBAhg4fNiQIMWJFhBAZTozI8aHBixIVdgTpUONHkR1TqrQ4EiTJkBtP
Dnx5UGNMjjZRlrxosiZNnSFxAl05MCAAOw=="]

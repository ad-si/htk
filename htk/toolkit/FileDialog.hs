{- --------------------------------------------------------------------
 -
 - Module FileDialog
 -
 - simple file dialog using list boxes
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}

module FileDialog (

  FileDialog(..),
  fileDialog,
--  modalFileDialog,
  fileChosen

) where

import HTk
import Concurrency
import ListBox
import ScrollBar
import Label
import Button
import CheckButton
import Entry
import Menu
import PulldownMenu
import Image
import Mouse
import Directory
import RVar
import System
import Channels
import Maybe

debug = False
debugMsg str = if debug then putStr (">>> " ++ str ++ "\n\n") else done

getFilesAndFolders :: FilePath -> Bool -> IO ([FilePath], [FilePath])
getFilesAndFolders path showhidden =
  do
    debugMsg "getting directory contents"
    dcontents <- getDirectoryContents path
    debugMsg "got directory contents"
    sort dcontents [] [] path
  where sort :: [FilePath] -> [FilePath] -> [FilePath] -> FilePath ->
                IO ([FilePath], [FilePath])
        sort (f : fs) files folders abs =
          if f == "." || f == ".." || (hidden f && not showhidden) then
            sort fs files folders abs
          else
            do
              debugMsg ("getting permissions of " ++ abs ++ f)
              p <- getPermissions (abs ++ f)
              debugMsg ("got permissions of " ++ abs ++ f)
              (if searchable p && readable p then
                 sort fs files ((f ++ "/") : folders) abs
               else sort fs (f : files) folders abs)
        sort _ files folders _ =
          return (sort' files, if path == "/" then sort' folders
                                              else ".." : (sort' folders))
        sort' :: [FilePath] -> [FilePath]
        sort' (f : fs) = sort'(filter (\f' -> f' < f) fs) ++ [f] ++
                         sort'(filter (\f' -> f' > f) fs)
        sort' _ = []
        hidden :: FilePath -> Bool
        hidden f = head f == '.'

dropLast :: FilePath -> FilePath
dropLast path = dropLast' (tail (reverse path))
  where dropLast' :: String -> String
        dropLast' (c : cs) =
          if c == '/' then reverse (c : cs) else dropLast' cs

updPathMenu :: MenuButton () -> RVar (Maybe (Menu ())) ->
               FilePath -> RVar [FilePath] -> RVar [FilePath] ->
               RVar FilePath -> ListBox [FilePath] ->
               ListBox [FilePath] -> Entry FilePath -> Label FilePath ->
               RVar Bool -> IO ()
updPathMenu pathmenubutton menuref path foldersref filesref pathref
            folderslb fileslb fileEntry status showhiddenref =
  do
    pathmenubutton # text path
    menu <- getVar menuref
    case menu of
      Just m -> destroy m
      _ -> done
    pathmenu <- newPulldownMenu pathmenubutton [tearOff Off]
    paths <- return (upperPaths path)
    mapM (createNewMenuItem pathmenu) paths
    setVar menuref (Just pathmenu)
  where upperPaths :: FilePath -> [FilePath]
        upperPaths "/" = ["/"]
        upperPaths p = p : upperPaths (dropLast p)
        createNewMenuItem :: Menu () -> FilePath -> IO ()
        createNewMenuItem pathmenu fp =
          do
            item <- newMenuItem pathmenu [text fp, command (selected fp)]
            interactor (const (triggered item))
            done
        selected :: FilePath -> () -> IO ()
        selected fp () =
          do
            status # value "Reading...     "
            showhidden <- getVar showhiddenref
            success <- changeToFolder fp foldersref filesref
                         pathref folderslb fileslb fileEntry showhidden
            (if success then
               do
                 status # value "Reading...ready"
                 nupath <- getVar pathref
                 updPathMenu pathmenubutton menuref nupath foldersref
                   filesref pathref folderslb fileslb fileEntry status
                   showhiddenref
             else
               status # value "Permission denied!" >> done)

changeToFolder :: FilePath -> RVar [FilePath] -> RVar [FilePath] ->
                  RVar FilePath -> ListBox [FilePath] ->
                  ListBox [FilePath] -> Entry FilePath -> Bool -> IO Bool
changeToFolder path foldersref filesref pathref folderslb fileslb
               fileEntry showhidden =
  do
    acc <- system ("access -rx " ++ path)
    (if acc == ExitSuccess then
       do
         setVar pathref path
         debugMsg "getting files and folders"
         (files, folders) <- getFilesAndFolders path showhidden
         debugMsg "got files and folders"
         setVar filesref files
         setVar foldersref folders
         folderslb # value folders
         fileslb # value files
         fileEntry # value ""
         return True
     else return False)

up ::  RVar [FilePath] -> RVar [FilePath] -> RVar FilePath ->
       ListBox [FilePath] -> ListBox [FilePath] -> Entry FilePath ->
       Label String -> Bool -> IO Bool
up foldersref filesref pathref folderslb fileslb fileEntry status
   showhidden =
  do
    path <- getVar pathref
    status # value "Reading...     "
    changeToFolder (dropLast path) foldersref filesref pathref
      folderslb fileslb fileEntry showhidden

selectedFolder :: Int -> RVar [FilePath] -> RVar [FilePath] ->
                  RVar FilePath -> ListBox [FilePath] ->
                  ListBox [FilePath] -> Entry FilePath -> Bool -> IO Bool
selectedFolder i foldersref filesref pathref folderslb fileslb fileEntry
               showhidden =
  do
    folders <- getVar foldersref
    path <- getVar pathref
    nupath <- return (if (folders !! i) == ".." then
                        dropLast path
                      else path ++ (folders !! i))
    changeToFolder nupath foldersref filesref pathref folderslb fileslb
      fileEntry showhidden

refresh :: RVar [FilePath] -> RVar [FilePath] -> RVar FilePath ->
           ListBox [FilePath] -> ListBox [FilePath] -> Bool -> IO ()
refresh foldersref filesref pathref folderslb fileslb showhidden =
  do
    folders <- getVar foldersref
    files <- getVar filesref
    path <- getVar pathref
    (files, folders) <- getFilesAndFolders path showhidden
    setVar filesref files
    setVar foldersref folders
    folderslb # value folders
    fileslb # value files
    done

selectFile :: Int -> RVar [FilePath] -> Entry String -> IO ()
selectFile i filesref fileEntry =
  do
    files <- getVar filesref
    fileEntry # value (files !! i)
    done

data FileDialog = FileDialog (MsgQueue (Maybe FilePath))

createFolder :: RVar (Maybe String) -> IO ()
createFolder ret =
  do
    main <- newVBox []
    win <- window main [text "New folder"]
    entnlab <- newHBox [pad Horizontal 10, pad Vertical 5, parent main]
    lab <- newLabel [font (Lucida, 12::Int), value "Name:",
                     parent entnlab]
    ent <- newEntry [bg "white", pad Horizontal 10, pad Vertical 5,
                     width 40, parent entnlab]
    buttons <- newHBox [pad Horizontal 10, pad Vertical 5, parent main]
    ok <- newButton [text "Ok", parent buttons, width 15,
                     pad Horizontal 5,
                     command (\ () -> do
                                        nm <- getValue ent
                                        setVar ret (Just nm)
                                        destroy win)]
    quit <- newButton [text "Cancel", parent buttons, width 15,
                       pad Horizontal 5,
                       command (\ () -> do
                                          setVar ret Nothing
                                          destroy win)]    
    interactor (\i -> triggered ok +> triggered quit)
    sync (destroyed win)

confirmDeleteFile :: FilePath -> RVar Bool -> IO ()
confirmDeleteFile fp ret =
  do
    main <- newVBox []
    win <- window main [text "Delete file"]
    lab <- newLabel [font (Lucida, 12::Int),
                     value ("Do you really want to delete the file '" ++
                            fp ++ "' ?"), pad Horizontal 10,
                     pad Vertical 5, parent main]
    buttons <- newHBox [pad Horizontal 10, pad Vertical 5, parent main]
    ok <- newButton [text "Ok", parent buttons, width 15,
                     pad Horizontal 5,
                     command (\ () -> setVar ret True >> destroy win)]
    quit <- newButton [text "Cancel", parent buttons, width 15,
                       pad Horizontal 5,
                       command (\ () -> setVar ret False >> destroy win)]
    interactor (\i -> triggered ok +> triggered quit)
    sync (destroyed win)

fileDialog :: String -> FilePath -> IO FileDialog
fileDialog title path' =
  do
    path <- return (if last path' == '/' then path' else path' ++ "/")
    main <- newVFBox []
    win <- window main [text title]
    (files, folders) <- getFilesAndFolders path False
    pathref <- newRVar path
    filesref <- newRVar files
    foldersref <- newRVar folders
    showhiddenref <- newRVar False
    actions <- newHFBox [pad Vertical 10, parent main]
    pathmenubutton <- newMenuButton [text path, width 50, relief Raised,
                                     pad Horizontal 10, parent actions]
    upImg' <- upImg
    refreshImg' <- refreshImg
    newFolderImg' <- newFolderImg
    deleteFileImg' <- deleteFileImg
    upbutton <- newButton [pad Horizontal 2, photo upImg', parent actions]
    refreshbutton <- newButton [pad Horizontal 2, photo refreshImg',
                                parent actions]
    newfolderbutton <- newButton [pad Horizontal 2, photo newFolderImg',
                                  parent actions]
    deletefilebutton <- newButton [pad Horizontal 2, photo deleteFileImg',
                                   parent actions]
    showHiddenFiles <- newCheckButton [pad Horizontal 10,
                                       text "Show hidden files",
                                       parent actions]
    menuref <- newRVar Nothing
    boxesnmsg <- newVFBox [parent main]
    boxes <- newHFBox [parent boxesnmsg]
    folderslist <- newHFBox [pad Horizontal 10, parent boxes]
    folderslb <- newListBox [value folders, size (35, 15), bg "white",
                             side AtLeft, font (Lucida, 12::Int),
                             parent folderslist]
    foldersscb <- newScrollBar [parent folderslist, side AtRight,
                                fill Vertical]
    folderslb # scrollbar Vertical foldersscb
    fileslist <- newHFBox [pad Horizontal 10, parent boxes]
    fileslb <- newListBox [value files, size (35, 15), bg "white",
                           side AtLeft, font (Lucida, 12::Int),
                           parent fileslist]
    filesscb <- newScrollBar [parent fileslist, side AtRight,
                              fill Vertical]
    fileslb # scrollbar Vertical filesscb
    status <- newLabel [value "Welcome", relief Sunken,
                        font (Lucida, 12::Int), pad Horizontal 10,
                        parent boxesnmsg]
    showHiddenFiles # command (\ () -> do
                                         s <- getVar showhiddenref
                                         setVar showhiddenref (not s)
                                         status # value "Reading...     "
                                         refresh foldersref filesref
                                           pathref folderslb fileslb
                                           (not s)
                                         status # value "Reading...ready"
                                         done)
    fileEntry <- newEntry [bg "white", pad Horizontal 60, pad Vertical 10,
                           parent main] :: IO (Entry String)
    updPathMenu pathmenubutton menuref path foldersref filesref pathref
      folderslb fileslb fileEntry status showhiddenref
    upbutton # command (\ () ->
                          do
                            showhidden <- getVar showhiddenref
                            success <- up foldersref filesref pathref
                                         folderslb fileslb fileEntry
                                         status showhidden
                            (if success then
                               do
                                 status # value "Reading...ready"
                                 nupath <- getVar pathref
                                 updPathMenu pathmenubutton menuref nupath
                                   foldersref filesref pathref folderslb
                                   fileslb fileEntry status showhiddenref
                             else status # value "Permission denied!" >>
                                           done))
    refreshbutton # command (\ () -> do
                                       showhidden <- getVar showhiddenref
                                       refresh foldersref filesref pathref
                                         folderslb fileslb showhidden)
    newfolderbutton #
      command
        (\ () ->
           do
             ret <- newRVar Nothing
             createFolder ret
             ret' <- getVar ret
             case ret' of
               Just nm ->
                 do
                   path <- getVar pathref
                   exitstatus <- system ("mkdir " ++ path ++ nm)
                   (if exitstatus == ExitSuccess then
                      do
                        status # value ("created folder " ++ nm)
                        showhidden <- getVar showhiddenref
                        refresh foldersref filesref pathref folderslb
                          fileslb showhidden
                    else
                      status # value ("Error: Couldn't create folder '" ++
                                      nm ++ "'") >> done)
               _ -> status # value "cancelled folder creation" >> done)
    deletefilebutton #
      command
        (\ () ->
           do
             nm <- getVar fileEntry
             (if nm == "" then
                status # value "no file selected" >> done
              else
                do
                  ret <- newRVar False
                  path <- getVar pathref
                  confirmDeleteFile (path ++ nm) ret
                  ret' <- getVar ret
                  (if ret' then
                     do
                       exitstatus <- system ("rm " ++ path ++ nm)
                       (if exitstatus == ExitSuccess then
                          do
                            status # value ("file '" ++ nm ++ "' deleted")
                            showhidden <- getVar showhiddenref
                            refresh foldersref filesref pathref folderslb
                              fileslb showhidden
                        else
                          status # value
                                     ("Error: Couldn't delete file '" ++
                                      nm ++ "'") >> done)
                   else status # value "cancelled file deletion" >>
                        done)))
    buttons <- newHFBox [pad Horizontal 110, parent main]
    msgQ <- newMsgQueue
    ok <- newButton [pad Vertical 10, pad Horizontal 5, text "Ok",
                     parent buttons,
                     command (\ () -> do
                                        fname <- getValue fileEntry
                                        path <- getVar pathref
                                        sendIO msgQ (Just (path ++ fname))
                                        destroy win)]
    quit <- newButton [pad Vertical 10, pad Horizontal 5, text "Cancel",
                       parent buttons,
                       command (\ () -> do
                                          sendIO msgQ Nothing
                                          destroy win)]
    interactor (\i -> triggered ok +> triggered quit +>
                      triggered upbutton +> triggered refreshbutton +>
                      triggered newfolderbutton +>
                      triggered deletefilebutton +>
                      triggered showHiddenFiles +>
                      (mouseButtonPress folderslb 1 >>>
                         do
                           sel <- getSelection
                                    folderslb :: IO (Maybe [Int])
                           case sel of
                             Just (i : _) ->
                               do
                                 showhidden <- getVar showhiddenref
                                 status # value "Reading...     "
                                 success <- selectedFolder i foldersref
                                              filesref pathref folderslb
                                              fileslb fileEntry showhidden
                                 (if success then
                                    do
                                      status # value "Reading...ready"
                                      nupath <- getVar pathref
                                      updPathMenu pathmenubutton menuref
                                        nupath foldersref filesref pathref
                                        folderslb fileslb fileEntry
                                        status showhiddenref
                                  else
                                    status # value "Permission denied!" >>
                                    done)
                             _ -> done) +>
                      (mouseButtonPress fileslb 1 >>>
                         do
                           sel <- getSelection fileslb :: IO (Maybe [Int])
                           case sel of
                             Just (i : _) ->
                               selectFile i filesref fileEntry
                             _ -> done))
    return (FileDialog msgQ)

fileChosen :: FileDialog -> IA (Maybe FilePath)
fileChosen (FileDialog msgQ) = lift(receive msgQ)

upImg = newImage [imgData GIF "R0lGODlhFAAUAJEDAP//////AAAAAP///yH5BAEAAAMALAAAAAAUABQAAAJFBIapm8cPo5y0VoHz
DbwLlYVCRwbfMJYl5n2pyoWxC88yhtamOfY0CwPycpngC5cSKVs525L50pFwKKU1lLheH4Zul1EA
ADs="]

refreshImg = newImage [imgData GIF "R0lGODlhFAAUAMQUAPj4+Pz8/Pv7+/b29gYGBvX19ZiYmPr6+oCAgAgICAcHB/Pz8/n5+QUFBYiI
iJaWlv39/f7+/v///wAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEA
ABQALAAAAAAUABQAAAWJoESNZGmeaEpGVBRBoxuJKQvHzzPtjwqxA91uOFFRIMLHbKQ73RY8Fw31
ggyGARaFVixFAAnirjT0fhXEJUW8cjEIOwFX3KVJBJODJJIdlUk0L14UAA5jgxQBBQ0GJQg8Jix7
LHQTPSYQe0dMQzk3JT8vAC1aVFssLhA3dy8uAVswUyiygEYqLCEAOw=="]

newFolderImg = newImage [imgData GIF "R0lGODlhFAAUAJEDAAAAAP//////AP///yH5BAEAAAMALAAAAAAUABQAAAJGjH+jyB26HAhzGhjZ
ABxh93BWMoDUOR6iaCKAAMfpusa2YL33bnc6z/MBgcLhrmjsVZI3JBO3fMKcTI6Chs1CstzJp8Uo
AAA7"]

deleteFileImg = newImage [imgData GIF "R0lGODlhFAAUAJECAP////8AAP///wAAACH5BAEAAAIALAAAAAAUABQAAAJKlI8Hy5CvmoOPhXUC
NSD4a3xb52GhB32XeWqI2jxoK00v+tX2LeqUiIglVECBMCNScRg8lwB2VA5LzJbzJiymVhOtjLoZ
7cIRQAEAOw=="]

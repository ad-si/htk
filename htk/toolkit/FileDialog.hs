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

---
-- HTk's <strong>file dialog box</strong>.
module FileDialog (

  fileDialog

) where

import HTk
import Directory
import Posix
import ReferenceVariables
import System
import Maybe
import TkVariables
import ModalDialog

tlDebug = False
debugMsg str = if tlDebug then putStr (">>> " ++ str ++ "\n") else done

getFilesAndFolders :: FilePath -> Bool -> IO ([FilePath], [FilePath])
getFilesAndFolders path showhidden =
  do
    debugMsg ("getting directory contents of " ++ path)
    dcontents <- getDirectoryContents path
    debugMsg "...ok\n"
    sort dcontents [] [] path
  where sort :: [FilePath] -> [FilePath] -> [FilePath] -> FilePath ->
                IO ([FilePath], [FilePath])
        sort (f : fs) files folders abs =
          if f == "." || f == ".." || (hidden f && not showhidden) then
            sort fs files folders abs
          else
            do
              debugMsg ("trying to get file status of " ++ abs ++ f)
              st' <- try (getFileStatus (abs ++ f))
              case st' of
                Left st -> debugMsg "...failed\n" >>
                           sort fs files folders abs
                Right st ->
                  debugMsg "...ok\n" >>
                  (if isDirectory st then
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
dropLast [] = []
dropLast path = dropLast' (tail (reverse path))
  where dropLast' :: String -> String
        dropLast' (c : cs) =
          if c == '/' then reverse (c : cs) else dropLast' cs
        dropLast' _ = []

updPathMenu :: MenuButton -> Ref (Maybe Menu) ->
               FilePath -> Ref [FilePath] -> Ref [FilePath] ->
               Ref FilePath -> ListBox FilePath ->
               ListBox FilePath -> TkVariable String ->
               Label -> Ref Bool -> IO ()
updPathMenu pathmenubutton menuref path foldersref filesref pathref
            folderslb fileslb file_var status showhiddenref =
  do
    pathmenubutton # text path
    m <- getRef menuref
    case m of
      Just m' -> destroy m'
      _ -> done
    pathmenu <- createMenu pathmenubutton False []
    pathmenubutton # menu pathmenu
    let paths = upperPaths path
    mapM (createNewMenuItem pathmenu) paths
    setRef menuref (Just pathmenu)
  where upperPaths :: FilePath -> [FilePath]
        upperPaths "/" = ["/"]
        upperPaths p = p : upperPaths (dropLast p)
        createNewMenuItem :: Menu -> FilePath -> IO ()
        createNewMenuItem pathmenu fp =
          do
            item <- createMenuCommand pathmenu [text fp]
            clickeditem <- clicked item
            spawnEvent (forever (clickeditem >> always (selected fp)))
            done
        selected :: FilePath -> IO ()
        selected fp =
          do
            status # text "Reading...     "
            showhidden <- getRef showhiddenref
            success <- changeToFolder fp foldersref filesref
                         pathref folderslb fileslb file_var showhidden
            (if success then
               do
                 status # text "Reading...ready"
                 nupath <- getRef pathref
                 updPathMenu pathmenubutton menuref nupath foldersref
                   filesref pathref folderslb fileslb file_var status
                   showhiddenref
             else
               status # text "Permission denied!" >> done)

changeToFolder :: FilePath -> Ref [FilePath] -> Ref [FilePath] ->
                  Ref FilePath -> ListBox FilePath ->
                  ListBox FilePath -> TkVariable String -> Bool ->
                  IO Bool
changeToFolder path foldersref filesref pathref folderslb fileslb
               file_var showhidden =
  do
    let path' = if path == "" then "/" else path
    acc <- system ("access -rx \"" ++ path' ++ "\"")
--    acc <- queryAccess path' True False True
    (if acc == ExitSuccess then
       do
         setRef pathref path
         debugMsg "getting files and folders"
         (files, folders) <- getFilesAndFolders path' showhidden
         debugMsg "got files and folders"
         setRef filesref files
         setRef foldersref folders
         folderslb # value folders
         fileslb # value files
         setTkVariable file_var ""
         return True
     else return False)

up ::  Ref [FilePath] -> Ref [FilePath] -> Ref FilePath ->
       ListBox FilePath -> ListBox FilePath -> TkVariable String ->
       Label -> Bool -> IO Bool
up foldersref filesref pathref folderslb fileslb file_var status
   showhidden =
  do
    path <- getRef pathref
    (if path /= "" && path /= "/" then
       do
         status # text "Reading...     "
         changeToFolder (dropLast path) foldersref filesref pathref
           folderslb fileslb file_var showhidden
     else return True)

selectedFolder :: Int -> Ref [FilePath] -> Ref [FilePath] ->
                  Ref FilePath -> ListBox FilePath ->
                  ListBox FilePath -> TkVariable String -> Bool ->
                  IO Bool
selectedFolder i foldersref filesref pathref folderslb fileslb file_var
               showhidden =
  do
    folders <- getRef foldersref
    path <- getRef pathref
    nupath <- return (if (folders !! i) == ".." then
                        dropLast path
                      else path ++ (folders !! i))
    changeToFolder nupath foldersref filesref pathref folderslb fileslb
      file_var showhidden

refresh :: Ref [FilePath] -> Ref [FilePath] -> Ref FilePath ->
           ListBox FilePath -> ListBox FilePath -> Bool -> IO ()
refresh foldersref filesref pathref folderslb fileslb showhidden =
  do
    folders <- getRef foldersref
    files <- getRef filesref
    path <- getRef pathref
    (files, folders) <- getFilesAndFolders path showhidden
    setRef filesref files
    setRef foldersref folders
    folderslb # value folders
    fileslb # value files
    done

selectFile :: Int -> Ref [FilePath] -> TkVariable String -> IO ()
selectFile i filesref file_var =
  do
    files <- getRef filesref
    setTkVariable file_var (files !! i)

createFolder :: Toplevel -> Ref (Maybe Toplevel) -> Ref (Maybe String) ->
                IO ()
createFolder par childwindow ret =
  synchronize par
    (do
       (w, h, x, y) <- getGeometry par
       let w' = 400
           h' = 100
       main <- createToplevel [text "Create a new folder",
                               geometry (w', h',
                                         x + (div w 2) - (div w' 2),
                                         y + (div h 2) - (div h' 2))]
       setRef childwindow (Just main)
       (main_destr, main_destr_ub) <- bindSimple main Destroy

       entnlab <- newFrame main []
       pack entnlab [PadX 10, PadY 5]

       lab <- newLabel entnlab [font (Lucida, 12::Int),
                                text "Enter name:"]
       pack lab []

       ent_var <- createTkVariable ""
       ent <- newEntry entnlab [bg "white", width 40, variable ent_var]
                :: IO (Entry String)
       pack ent [PadX 10, PadY 5]

       buttons <- newFrame main []
       pack buttons [PadX 10, PadY 5, Side AtBottom]

       ok <- newButton buttons [text "Ok", width 12]
       pack ok [PadX 5, Side AtLeft]

       quit <- newButton buttons [text "Cancel", width 12]
       pack quit [PadX 5, Side AtLeft]

       clickedok <- clicked ok
       clickedquit <- clicked quit

       let cleanUp :: IO ()
           cleanUp = main_destr_ub >> setRef childwindow Nothing

           listenDialog :: Event ()
           listenDialog =
                (clickedquit >> always (cleanUp >> destroy main))
             +> (clickedok >> always (do
                                        cleanUp
                                        nm <- readTkVariable ent_var
                                        setRef ret (Just nm)
                                        destroy main))
             +> (main_destr >> always (cleanUp))

       modalDialog main True listenDialog)

confirmDeleteFile :: Toplevel -> FilePath -> Ref (Maybe Toplevel) ->
                     Ref Bool -> IO ()
confirmDeleteFile par fp childwindow ret =
  synchronize par
    (do
       (w, h, x, y) <- getGeometry par
       let w' = 400
           h' = 100
       main <- createToplevel [text "Delete file",
                               geometry (w', h',
                                         x + (div w 2) - (div w' 2),
                                         y + (div h 2) - (div h' 2))]
       setRef childwindow (Just main)
       (main_destr, main_destr_ub) <- bindSimple main Destroy

       lab <- newLabel main
                [font (Lucida, 12::Int),
                 text ("Do you really want to delete the file \n'" ++
                       fp ++ "' ?")]
       pack lab [PadX 10, PadY 5]

       buttons <- newFrame main []
       pack buttons [PadX 10, PadY 5, Side AtBottom]

       ok <- newButton buttons [text "Ok", width 15]
       pack ok [PadX 5, Side AtLeft]

       quit <- newButton buttons [text "Cancel", width 15]
       pack quit [PadX 5, Side AtLeft]

       clickedok <- clicked ok
       clickedquit <- clicked quit

       let cleanUp :: IO ()
           cleanUp = main_destr_ub >> setRef childwindow Nothing

           listenDialog :: Event ()
           listenDialog =
                (clickedok >> always (cleanUp >> setRef ret True >>
                                      destroy main))
             +> (clickedquit >> always (setRef ret False >> cleanUp >>
                                        destroy main))
             +> (main_destr >> always (cleanUp))

       modalDialog main True listenDialog)

---
-- Opens a file dialog box.
-- @param title   - the window title of the file dialog box.
-- @param fp      - the filepath to browse.
-- @return result - An event (returning the selected FilePath if
--                  available) that is invoked when the file dialog is
--                  finished.
fileDialog :: String -> FilePath -> IO (Event (Maybe FilePath))
fileDialog title fp =
  do
    let path = if last fp == '/' then fp else fp ++ "/"

    childwindow <- newRef Nothing

    main <- createToplevel [text title]
    (main_destr, main_destr_ub) <- bindSimple main Destroy

    let w' = 680
        h' = 400
    w <- getScreenWidth (Screen main)
    h <- getScreenHeight (Screen main)
    main # geometry (w', h', (div w 2) - (div w' 2),
                     (div h 2) - (div h' 2))

    (files, folders) <- getFilesAndFolders path False

    pathref <- newRef path
    filesref <- newRef files
    foldersref <- newRef folders
    showhiddenref <- newRef False

    actions <- newFrame main []
    pack actions [PadY 10]

    pathmenubutton <- newMenuButton actions [text path, width 50,
                                             relief Raised]
    pack pathmenubutton [PadX 10, Side AtLeft]

    upImg' <- upImg
    refreshImg' <- refreshImg
    newFolderImg' <- newFolderImg
    deleteFileImg' <- deleteFileImg

    upbutton <- newButton actions [photo upImg']
    pack upbutton [PadX 2, Side AtLeft]

    refreshbutton <- newButton actions [photo refreshImg']
    pack refreshbutton [PadX 2, Side AtLeft]

    newfolderbutton <- newButton actions [photo newFolderImg']
    pack newfolderbutton [PadX 2, Side AtLeft]

    deletefilebutton <- newButton actions [photo deleteFileImg']
    pack deletefilebutton [PadX 2, Side AtLeft]

    showHiddenFiles <- newCheckButton actions [text "hidden files"]
    pack showHiddenFiles [PadX 10, Side AtLeft]

    menuref <- newRef Nothing

    boxesnmsg <- newFrame main []
    pack boxesnmsg [PadX 10]

    boxes <- newFrame boxesnmsg []
    pack boxes [PadX 10, Fill X, Expand On]
    folderslist <- newFrame boxes []
    pack folderslist [Side AtLeft]
    folderslb <- newListBox folderslist [value folders, size (35, 15),
                                         bg "white",
                                         font (Lucida, 12::Int)]
    pack folderslb [Side AtLeft]
    foldersscb <- newScrollBar folderslist []
    pack foldersscb [Side AtRight, Fill Y]
    folderslb # scrollbar Vertical foldersscb
    fileslist <- newFrame boxes []
    pack fileslist [Side AtRight]
    fileslb <- newListBox fileslist [value files, size (35, 15),
                                     bg "white", font (Lucida, 12::Int)]
    pack fileslb [Side AtLeft]
    filesscb <- newScrollBar fileslist []
    pack filesscb [Side AtRight, Fill Y]
    fileslb # scrollbar Vertical filesscb
    status <- newLabel boxesnmsg [text "Welcome", relief Raised,
                                  font (Lucida, 12::Int), anchor Center]
    pack status [PadX 10, Fill X, Expand On]

    file_var <- createTkVariable ""
    fileEntry <- newEntry main [bg "white", variable file_var]
                   :: IO (Entry String)
    pack fileEntry [PadX 50, PadY 5, Fill X, Expand On]

    updPathMenu pathmenubutton menuref path foldersref filesref pathref
      folderslb fileslb file_var status showhiddenref

    buttons <- newFrame main []
    pack buttons [PadY 5, PadX 30, Side AtRight]
    msgQ <- newChannel
    ok <- newButton buttons [text "Ok", width 12]
    pack ok [PadX 5, Side AtLeft]

    quit <- newButton buttons [text "Cancel", width 12]
    pack quit [PadX 5, Side AtRight]


    -- events
    clickeddeletefilebutton <- clicked deletefilebutton
    clickedshowHiddenFiles <- clicked showHiddenFiles
    clickedupbutton <- clicked upbutton
    clickedrefreshbutton <- clicked refreshbutton
    clickednewfolderbutton <- clicked newfolderbutton
    clickedok <- clicked ok
    clickedquit <- clicked quit
    (fbpress, fbpress_ub) <- bindSimple folderslb
                              (ButtonPress (Just 1))
    (flpress, flpress_ub) <- bindSimple fileslb
                               (ButtonPress (Just 1))

    let cleanUp :: IO ()
        cleanUp = flpress_ub >> fbpress_ub >> main_destr_ub

        listenDialog :: Event ()
        listenDialog =
             (flpress >> always
                           (do
                              sel <- getSelection fileslb
                                       :: IO (Maybe [Int])
                              case sel of
                                Just (i : _) ->
                                  selectFile i filesref file_var
                                _ -> done) >>
              listenDialog)
          +> (fbpress >> always
                           (do
                              sel <- getSelection
                                       folderslb :: IO (Maybe [Int])
                              case sel of
                                Just (i : _) ->
                                  do
                                    showhidden <- getRef showhiddenref
                                    status # text "Reading...     "
                                    success <- selectedFolder i foldersref
                                                 filesref pathref
                                                 folderslb fileslb
                                                 file_var showhidden
                                    (if success then
                                       do
                                         status # text "Reading...ready"
                                         nupath <- getRef pathref
                                         updPathMenu pathmenubutton
                                           menuref nupath foldersref
                                           filesref pathref folderslb
                                           fileslb file_var status
                                           showhiddenref
                                         done
                                     else
                                       status #
                                         text "Permission denied!" >>
                                       done)
                                _ -> done) >>
              listenDialog)
          +> (clickedquit >> always (syncNoWait (send msgQ Nothing) >>
                                     cleanUp >> destroy main))
          +> (clickedok >> always (do
                                     fname <- readTkVariable file_var
                                     path <- getRef pathref
                                     syncNoWait
                                       (send msgQ (Just (path ++ fname)))
                                     cleanUp
                                     destroy main))
          +> (clickednewfolderbutton >>
              always
                (do
                   ret <- newRef Nothing
                   createFolder main childwindow ret
                   ret' <- getRef ret
                   case ret' of
                     Just nm ->
                       do
                         path <- getRef pathref
                         ok <- try (Directory.createDirectory
                                      (path ++ nm))
                         case ok of
                           Right _ ->
                             do
                               status #
                                 text ("created folder " ++ nm)
                               showhidden <- getRef showhiddenref
                               refresh foldersref filesref pathref
                                 folderslb fileslb showhidden
                           Left _ ->
                             status #
                               text
                                 ("Error: Couldn't create folder '" ++
                                  nm ++ "'") >>
                             done
                     _ -> status #
                            text "cancelled folder creation" >> done) >>
              listenDialog)
          +> (clickedrefreshbutton >>
              always (do
                        showhidden <- getRef showhiddenref
                        refresh foldersref filesref pathref
                          folderslb fileslb showhidden) >>
              listenDialog)
          +> (clickedupbutton >>
              always (do
                        showhidden <- getRef showhiddenref
                        success <- up foldersref filesref pathref
                                     folderslb fileslb file_var status
                                     showhidden
                        (if success then
                           do
                             status # text "Reading...ready"
                             nupath <- getRef pathref
                             updPathMenu pathmenubutton menuref nupath
                               foldersref filesref pathref folderslb
                               fileslb file_var status showhiddenref
                             done
                         else status # text "Permission denied!" >>
                              done)) >>
              listenDialog)
          +> (clickedshowHiddenFiles >>
              always (do
                        s <- getRef showhiddenref
                        setRef showhiddenref (not s)
                        status # text "Reading...     "
                        refresh foldersref filesref pathref folderslb
                          fileslb (not s)
                        status # text "Reading...ready"
                        done) >>
              listenDialog)
          +> (clickeddeletefilebutton >>
              always
                (do
                   nm <- readTkVariable file_var
                   (if nm == "" then
                      status # text "no file selected" >> done
                    else
                      do
                        ret <- newRef False
                        path <- getRef pathref
                        confirmDeleteFile main (path ++ nm) childwindow
                                          ret
                        ret' <- getRef ret
                        (if ret' then
                           do
                             ok <- try (removeFile (path ++ nm))
                             case ok of
                               Right _ ->
                                 do
                                   status #
                                     text ("file '" ++ nm ++ "' deleted")
                                   showhidden <- getRef showhiddenref
                                   refresh foldersref filesref pathref
                                     folderslb fileslb showhidden
                               Left _ ->
                                 status #
                                   text
                                     ("Error: Could not delete file '" ++
                                      nm ++ "'") >> done
                         else status # text "cancelled file deletion" >>
                              done))) >>
              listenDialog)
    spawnEvent listenDialog

    spawnEvent (main_destr >> always (do
                                        mchildwindow <- getRef childwindow
                                        case mchildwindow of
                                          Just win -> destroy win
                                          _ -> done
                                        cleanUp
                                        syncNoWait (send msgQ Nothing)))

    return (receive msgQ)

upImg = newImage NONE [imgData GIF "R0lGODlhFAAUAKEAAP//////AAAAAP///yH5BAEAAAMALAAAAAAUABQAAAJAnI+py+0Po1Si2iiC
3gLZn21iN4TiWXGdeWqfu7bqW5WyG6RZvbOjyculWkOhTQh6wY7I5I95Q5GSVNChWp0oCgA7"]

refreshImg = newImage NONE [imgData GIF "R0lGODlhFAAUAIQAAPj4+Pz8/Pv7+/b29gYGBvX19ZiYmPr6+oCAgAgICAcHB/Pz8/n5+QUFBYiI
iJaWlv39/f7+/v///wAAAP///////////////////////////////////////////////yH5BAEA
AB8ALAAAAAAUABQAAAU74CeOZGmeaKqu4+Q+rOjOLvuSz6TWccuXOlOC9vvMTgoaiXgiFInF1unY
kwVRDdNtB4XFqNWweEwWhQAAOw=="]

newFolderImg = newImage NONE [imgData GIF "R0lGODlhFAAUAKEAAAAAAP//////AP///yH5BAEAAAMALAAAAAAUABQAAAI5nI+pywjzXlOgzlXl
PRHSbG2AQJYaBGblKkgjC6/WG8dzXd84rO9y5GP1gi0gkTQMhlLMJqcJ3TQKADs="]

deleteFileImg = newImage NONE [imgData GIF "R0lGODlhFAAUAKEAAP////8AAP///////yH5BAEAAAAALAAAAAAUABQAAAIyhI+py+0WUnShTmBp
lVvZi2ShyHSY2WTk84HP6Wrt+8HxaNaLju/rgYIEOZwbcPhKPgoAOw=="]

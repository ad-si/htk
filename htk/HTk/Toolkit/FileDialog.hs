{-# LANGUAGE ScopedTypeVariables #-}

-- | HTk\'s /file dialog box/.
module HTk.Toolkit.FileDialog (
  fileDialogStr,
  newFileDialogStr,

  fileDialog,
  newFileDialog
) where


import System.Directory as Directory
import System.IO.Error

import qualified Data.List as List(sort)

import Control.Exception

import Util.FileNames
import Util.Messages
import Util.Computation

import Events.Events
import Events.Channels
import Events.Synchronized

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk
import HTk.Toolkit.ModalDialog (modalDialog)

debugMsg :: String-> IO ()
debugMsg str = done -- putStr (">>> " ++ str ++ "\n")


-- Display a warning window with a meaningful error message
ioErrorWindow :: SomeException -> IO ()
ioErrorWindow excep =
  warningMess ("Error while reading directory:\n"++
     case fromException excep of
        Just ioe ->
           ioeGetErrorString ioe++"\n"++
           case ioeGetFileName ioe of
                Just fn -> "with file "++fn++"\n"
                Nothing -> ""
        Nothing -> "Exception: "++show excep++"\n"
    )

tryGetFilesAndFolders :: FilePath -> Bool -> IO (Either SomeException
                                                        ([FilePath], [FilePath]))
tryGetFilesAndFolders path showhidden =
  do
    debugMsg ("getting directory contents of " ++ path)
    dc <- Control.Exception.try (getDirectoryContents path)
    case dc of
       Left exn -> do debugMsg "... error!"
                      return (Left exn)
       Right dcontents -> do debugMsg "...ok\n"
                             c<- sort dcontents [] [] path
                             return (Right c)
  where sort :: [FilePath] -> [FilePath] -> [FilePath] -> FilePath ->
                IO ([FilePath], [FilePath])
        sort (f : fs) files folders abs =
          if f == "." || f == ".." || (hidden f && not showhidden) then
            sort fs files folders abs
          else
            do
              fileIsDir <- doesDirectoryExist (abs ++ f)
              if fileIsDir
                 then
                    sort fs files ((f ++ "/") : folders) abs
                 else
                    sort fs (f : files) folders abs
        sort _ files folders _ =
          return (List.sort files,
                  if path == "/" then List.sort folders
                                 else ".." : (List.sort folders))
        hidden :: FilePath -> Bool
        hidden f = head f == '.'

getFilesAndFolders  :: FilePath -> Bool -> IO ([FilePath], [FilePath])
getFilesAndFolders path showhidden =
  do dc <- tryGetFilesAndFolders path showhidden
     case dc of
       Left ioe-> do ioErrorWindow ioe
                     return ([], [".."])
       Right cont-> return cont


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
            _ <- spawnEvent (forever (clickeditem >> always (selected fp)))
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
  let path' = if path == "" then "/" else path
  in  do debugMsg "getting files and folders"
         st <- tryGetFilesAndFolders path' showhidden
         case st of
           Right (files, folders) ->
             do setRef pathref path
                debugMsg "got files and folders"
                setRef filesref files
                setRef foldersref folders
                fileslb # value files
                folderslb # value folders
                setTkVariable file_var ""
                return True
           Left excep ->
              case fromException excep of
                 Just error | isPermissionError error -> return False
                 Nothing ->
                    do
                       ioErrorWindow excep
                       return False

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
    let
       trimmedPath = trimDir path

       nupath = if (folders !! i) == ".."
          then
             dropLast trimmedPath
          else
             combineNames trimmedPath (folders !! i)
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

       lab <- newLabel entnlab [font (Lucida, 12::Int),
                                text "Enter name:"]

       ent_var <- createTkVariable ""
       ent <- newEntry entnlab [bg "white", width 40, variable ent_var]
                :: IO (Entry String)
       buttons <- newFrame main []
       ok <- newButton buttons [text "Ok", width 12]
       quit <- newButton buttons [text "Cancel", width 12]

       pack entnlab [PadX 10, PadY 5]
       pack lab []
       pack ent [PadX 10, PadY 5]
       pack buttons [PadX 10, PadY 5, Side AtBottom]
       pack ok [PadX 5, Side AtLeft]
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


-- | Opens a file dialog box for a file which is to be created.
newFileDialogStr :: String
   -- ^ the window title of the file dialog box.
   -> FilePath
   -- ^ the filepath to browse.
   -> IO (Event (Maybe FilePath))
   -- ^ An event (returning the selected FilePath if
   -- available) that is invoked when the file dialog is
   -- finished.
newFileDialogStr title fp = do pr <- newRef fp
                               fileDialog' True title pr

-- | Opens a file dialog box for a file which should already exist.
fileDialogStr :: String
   -- ^ the window title of the file dialog box.
   -> FilePath
   -- ^ the filepath to browse.
   -> IO (Event (Maybe FilePath))
   -- ^ An event (returning the selected FilePath if
   -- available) that is invoked when the file dialog is
   -- finished.
fileDialogStr title fp = do pr <- newRef fp
                            fileDialog' False title pr

-- | Opens a file dialog box for a file which is to be created.
newFileDialog :: String
   -- ^ the window title of the file dialog box.
   -> Ref FilePath
   -- ^ refernce to filepath to browse.
   -> IO (Event (Maybe FilePath))
   -- ^ An event (returning the selected FilePath if
   -- available) that is invoked when the file dialog is
   -- finished.
newFileDialog = fileDialog' True

-- | Opens a file dialog box for a file which should already exist.
fileDialog :: String
   -- ^ the window title of the file dialog box.
   -> Ref FilePath
   -- ^ reference to filepath to browse.
   -> IO (Event (Maybe FilePath))
   -- ^ An event (returning the selected FilePath if
   -- available) that is invoked when the file dialog is
   -- finished.
fileDialog = fileDialog' False


-- | Opens a file dialog box.
fileDialog' :: Bool
   -- ^ True if the file is new, False if it should already exist.
   -> String
   -- ^ the window title of the file dialog box.
   -> Ref FilePath
   -- ^ reference to the filepath to browse.
   -> IO (Event (Maybe FilePath))
   -- ^ An event (returning the selected FilePath if
   -- available) that is invoked when the file dialog is
   -- finished.
fileDialog' isNew title pathref =
  do
    fp <- getRef pathref
    -- check wether we got a directory or directory/filename
    isDir <- doesDirectoryExist fp
    let (path,fn) =
            if isDir
            then (if last fp == '/' then fp else fp ++ "/","")
            else (\ (x,y) -> (x++"/",y)) (splitName fp)
    setRef pathref path
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

{-  pathref <- newRef path -}
    filesref <- newRef files
    foldersref <- newRef folders
    showhiddenref <- newRef False

    actions <- newFrame main []

    pathmenubutton <- newMenuButton actions [text path, width 50,
                                             relief Raised]

    upImg' <- upImg
    refreshImg' <- refreshImg
    newFolderImg' <- newFolderImg
    deleteFileImg' <- deleteFileImg

    upbutton <- newButton actions [photo upImg']
    refreshbutton <- newButton actions [photo refreshImg']
    newfolderbutton <- newButton actions [photo newFolderImg']
    deletefilebutton <- newButton actions [photo deleteFileImg']
    showHiddenFiles <- newCheckButton actions [text "hidden files"]
    menuref <- newRef Nothing

    boxesnmsg <- newFrame main []

    boxes <- newFrame boxesnmsg []
    folderslist <- newFrame boxes []
    folderslb <- newListBox folderslist [value folders, size (35, 15),
                                         bg "white",
                                         font (Lucida, 12::Int)]
    foldersscb <- newScrollBar folderslist []
    fileslist <- newFrame boxes []
    fileslb <- newListBox fileslist [value files, size (35, 15),
                                     bg "white", font (Lucida, 12::Int)]
    filesscb <- newScrollBar fileslist []
    status <- newLabel boxesnmsg [text "Welcome", relief Raised,
                                  font (Lucida, 12::Int), anchor Center]
    file_var <- createTkVariable fn
    fileEntry <- newEntry main [bg "white", variable file_var]
                   :: IO (Entry String)
    buttons <- newFrame main []
    ok <- newButton buttons [text "Ok", width 12]
    quit <- newButton buttons [text "Cancel", width 12]
    msgQ <- newChannel

    pack actions [PadY 10]
    pack pathmenubutton [PadX 10, Side AtLeft]
    pack upbutton [PadX 2, Side AtLeft]
    pack refreshbutton [PadX 2, Side AtLeft]
    pack newfolderbutton [PadX 2, Side AtLeft]
    pack deletefilebutton [PadX 2, Side AtLeft]
    pack showHiddenFiles [PadX 10, Side AtLeft]
    pack boxesnmsg [PadX 10, Expand On]

    pack boxes [PadX 10, Fill X, Expand On]
    pack folderslist [Side AtLeft, Expand Off]
    pack folderslb [Side AtLeft]
    pack foldersscb [Side AtRight, Fill Y]
    pack fileslist [Side AtRight, Expand On]

    folderslb # scrollbar Vertical foldersscb
    pack fileslb [Side AtLeft]
    pack filesscb [Side AtRight, Fill Y]
    fileslb # scrollbar Vertical filesscb
    pack status [PadX 10, PadY 3, Fill X, Expand On]

    pack fileEntry [PadX 50, PadY 5, Fill X, Expand On]
    pack buttons [PadY 5, PadX 30, Side AtRight]

    updPathMenu pathmenubutton menuref path foldersref filesref pathref
      folderslb fileslb file_var status showhiddenref

    pack ok [PadX 5, Side AtLeft]
    pack quit [PadX 5, Side AtRight]


    -- events
    clickeddeletefilebutton <- clicked deletefilebutton
    clickedshowHiddenFiles <- clicked showHiddenFiles
    clickedupbutton <- clicked upbutton
    clickedrefreshbutton <- clicked refreshbutton
    clickednewfolderbutton <- clicked newfolderbutton
    clickedok <- clicked ok
    clickedquit <- clicked quit
    (fbpress, fbpress_ub) <- bindSimple folderslb (ButtonPress (Just 1))
    (flpress, flpress_ub) <- bindSimple fileslb (ButtonPress (Just 1))
    (enterName, en_ub) <- bindSimple fileEntry (KeyPress (Just (KeySym "Return")))

    let cleanUp :: IO ()
        cleanUp = flpress_ub >> fbpress_ub >> main_destr_ub

        -- What to do when the user presses "OK" or "Return".   Returns True
        -- if we have successfully selected a file; False if we change
        -- directory or the user cancels.
        doFile :: Event ()
        doFile =
           always (
              do
                 quit <- doFileInner
                 if quit
                    then
                       do
                          cleanUp
                          destroy main
                    else
                       sync listenDialog
              )

        doFileInner :: IO Bool
        doFileInner =
           do
              file_nm <- readTkVariable file_var
              path <- getRef pathref
              let
                 trimmedPath = trimDir path
                    -- probably completely unnecessary, but I can't be
                    -- bothered to decrypt Andre's logic here.
                 fullnm= case file_nm of
                   '/':_ -> file_nm
                   _ -> combineNames trimmedPath file_nm

              fileIsDir <- doesDirectoryExist fullnm
              let
                 sendFile = syncNoWait (send msgQ (Just fullnm))

                 reset = setTkVariable file_var ""

              if fileIsDir
                 then
                   do
                      showhidden <- getRef showhiddenref
                      status # text "Reading...     "
                      success <- changeToFolder fullnm foldersref
                                 filesref pathref
                                 folderslb fileslb
                                 file_var showhidden
                      (if success then
                         do status # text "Reading...ready"
                            nupath <- getRef pathref
                            updPathMenu pathmenubutton
                                  menuref nupath foldersref
                                  filesref pathref folderslb
                                  fileslb file_var status
                                  showhiddenref
                            done
                         else
                           do status # text "Permission denied!"
                              done)
                      return False
                   else
                      do
                         fileExists <- doesFileExist fullnm
                         if fileExists
                            then
                               if isNew
                                  then
                                     do
                                        proceed <- confirmMess
                                           "File exists.  Overwrite?"

                                        if proceed then sendFile else reset
                                        return proceed
                                  else
                                     do
                                        sendFile
                                        return True
                            else
                               if isNew
                                  then
                                     do
                                        sendFile
                                        return True
                                  else
                                     do
                                        warningMess
                                           ("No such file or directory: "++
                                              fullnm)
                                        reset
                                        return False

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
                                       do status # text "Permission denied!"
                                          done)
                                _ -> done) >>
              listenDialog)
          +> (clickedquit >> always (syncNoWait (send msgQ Nothing) >>
                                     cleanUp >> destroy main))
          +> (clickedok >> doFile)
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
                         ok <- Control.Exception.try (Directory.createDirectory
                                      (path ++ nm))
                         case ok of
                           Right _ ->
                             do
                               status #
                                 text ("created folder " ++ nm)
                               showhidden <- getRef showhiddenref
                               refresh foldersref filesref pathref
                                 folderslb fileslb showhidden
                           Left (_ :: SomeException) ->
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
          +> (do clickedshowHiddenFiles
                 always (do s <- getRef showhiddenref
                            setRef showhiddenref (not s)
                            status # text "Reading...     "
                            refresh foldersref filesref pathref folderslb
                                    fileslb (not s)
                            status # text "Reading...ready"
                            done)
                 listenDialog)
          +> (do
                 enterName
                 doFile
              )
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
                             ok <- Control.Exception.try (removeFile (path ++ nm))
                             case ok of
                               Right _ ->
                                 do
                                   status #
                                     text ("file '" ++ nm ++ "' deleted")
                                   showhidden <- getRef showhiddenref
                                   refresh foldersref filesref pathref
                                     folderslb fileslb showhidden
                               Left (_ :: SomeException) ->
                                 status #
                                   text
                                     ("Error: Could not delete file '" ++
                                      nm ++ "'") >> done
                         else status # text "cancelled file deletion" >>
                              done))) >>
              listenDialog)
    _ <- spawnEvent listenDialog
    _ <- spawnEvent (main_destr >> always (do
                                        mchildwindow <- getRef childwindow
                                        case mchildwindow of
                                          Just win -> destroy win
                                          _ -> done
                                        cleanUp
                                        syncNoWait (send msgQ Nothing)))

    return (receive msgQ)

upImg = newImage [imgData GIF
 "R0lGODlhFAAUAKEAAP//////AAAAAP///yH5BAEAAAMALAAAAAAUABQAAAJAnI+py+0Po1Si2iiC3gLZn21iN4TiWXGdeWqfu7bqW5WyG6RZvbOjyculWkOhTQh6wY7I5I95Q5GSVNChWp0oCgA7"]

refreshImg = newImage [imgData GIF
 "R0lGODlhFAAUAIQAAPj4+Pz8/Pv7+/b29gYGBvX19ZiYmPr6+oCAgAgICAcHB/Pz8/n5+QUFBYiIiJaWlv39/f7+/v///wAAAP///////////////////////////////////////////////yH5BAEAAB8ALAAAAAAUABQAAAU74CeOZGmeaKqu4+Q+rOjOLvuSz6TWccuXOlOC9vvMTgoaiXgiFInF1unYkwVRDdNtB4XFqNWweEwWhQAAOw=="]

newFolderImg = newImage [imgData GIF
 "R0lGODlhFAAUAKEAAAAAAP//////AP///yH5BAEAAAMALAAAAAAUABQAAAI5nI+pywjzXlOgzlXlPRHSbG2AQJYaBGblKkgjC6/WG8dzXd84rO9y5GP1gi0gkTQMhlLMJqcJ3TQKADs="]

deleteFileImg = newImage [imgData GIF
 "R0lGODlhFAAUAKEAAP////8AAP///////yH5BAEAAAAALAAAAAAUABQAAAIyhI+py+0WUnShTmBplVvZi2ShyHSY2WTk84HP6Wrt+8HxaNaLju/rgYIEOZwbcPhKPgoAOw=="]

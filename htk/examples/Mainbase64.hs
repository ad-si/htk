{- ------------------------------------------------------------------------
 -
 - Base64 ecoding example
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Concurrency
import Image
import TreeList
import Button
import CheckButton
import Editor
import ScrollBox
import Maybe
import IO
import Directory
--import Base64

msg :: Editor String -> String -> IO ()
msg ed txt =
  do
    ed # state Normal
    appendText ed (txt ++ "\n")
    ed # state Disabled
    done

getMatchedFiles :: [FilePath] -> FilePath -> IO [FilePath]
getMatchedFiles fs abs = getMatchedFiles' fs [] [] abs
  where getMatchedFiles' :: [FilePath] -> [FilePath] -> [FilePath] ->
                            FilePath -> IO [FilePath]
        getMatchedFiles' (f : fs) dirs files abs =
          if ((f == ".") || (f == "..")) then
            do
              putStr "1\n"
              r <- getMatchedFiles' fs dirs files abs
              return r
          else
            do
              putStr ("getting permissions of " ++ f  ++ "\n")
              b <- getPermissions (abs ++ "/" ++ f)
              putStr "finished\n"
              (if not(hidden f)
               then putStr "searchable ?\n" >> if searchable b
                    then putStr "2\n" >>
                         getMatchedFiles' fs (f : dirs) files abs
                    else if gif f
                         then putStr "3\n" >> getMatchedFiles' fs dirs (f : files) abs
                         else putStr "4\n" >> getMatchedFiles' fs dirs files abs
               else putStr "5\n" >> getMatchedFiles' fs dirs files abs)
        getMatchedFiles' _ dirs files _ = putStr "6\n" >> return (dirs ++ files)
        hidden :: FilePath -> Bool
        hidden ('.':_) = True
        hidden _ = False

gif :: FilePath -> Bool
gif f = gif' (reverse f)
  where gif' :: String -> Bool
        gif' ('f' : 'i' : 'g' : '.' : _) = True
        gif' ('F' : 'I' : 'G' : '.' : _) = True
        gif' _ = False

dropQuotes :: String -> String
dropQuotes str =
  drop 1 (take (length str - 1) str)

{-
toTreeListObjects :: TreeList String -> String -> [FilePath] ->
                     IO [TreeListObject String]
toTreeListObjects treelist path (f : fs) =
  do
    obj <- newTreeListObject treelist (path ++ "/" ++ dropQuotes(show f))
                             [name (dropQuotes (show f))]
    objs <- toTreeListObjects treelist path fs
    return (obj : objs)
toTreeListObjects _ _ _ = return []
-}

toTreeListObjects :: TreeList String -> String -> [FilePath] ->
                     IO [TreeListObject String]
toTreeListObjects treelist path (f : fs) =
  do
    obj <- return (treelist, path ++ "/" ++ dropQuotes(show f),
                   dropQuotes (show f))
    objs <- toTreeListObjects treelist path fs
    return (obj : objs)
toTreeListObjects _ _ _ = return []

printcontents (f : fs) = putStr (f ++ "\n") >> printcontents fs
printcontents _ = done

cfun :: ChildrenFun String
cfun (tl, val, nm) =
  do
    putStr ("cfun of " ++ val ++ "\n")
    (if gif val then
       do
         putStr "is gif\n\n"
         return []
     else
       do
         dcontents <- getDirectoryContents val
         printcontents dcontents
         putStr "contents read\n"
         matched_files <- getMatchedFiles dcontents val
         putStr "contents sorted\n"
         objs <- toTreeListObjects tl val matched_files
         putStr "finished\n\n"
         return objs)

ifun :: ImageFun String
ifun (tl, val, nm) =
  do
    b <- getPermissions val
    if searchable b then folderImg else fileImg

main =
  do
    win <- htk []
    main <- newHFBox []
    box <- newVBox [parent main]
    win <- window main [text "Base64 encoding"]
    treelist <- newTreeList cfun ifun [parent box, background "white",
                                       size (cm 9, cm 10)]
    setRoot (treelist, "/", "/")
    show_hidden <- newCheckButton [text "Show hidden files", parent box]
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent box, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    base64out <- newEditor [width 60, height 15, state Disabled] :: IO (Editor String)
    base64_box <- newScrollBox base64out [parent main]
{-    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> do
                                    objname <- case mobj of
                                                 Just obj -> getName obj
                                                 _ -> return "Nothing"
                                    msg base64out (objname ++ " selected")))
-}
    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> msg base64out
                                      ((case mobj of
                                          Just (_, _, nm) -> nm
                                          _ -> "Nothing") ++ " selected")))
    sync (destroyed win)

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]

fileImg = newImage [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMcAAAAjANz/AAD//wCZmZn///8AAAD///8AAAAA
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
SgALCBxIcCCBgQMSDihAAADDAgEiChAgsOFDiAEIHryo0KBDAiBDivwosmRFjAEoUtx4UEDGhSs/
Flg4QEBChjIRehQIoKfPnwEBADs="]

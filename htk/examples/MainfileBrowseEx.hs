{- ------------------------------------------------------------------------
 -
 - file browse example (as an example for the treelist module)
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
import Posix
import System

msg :: Editor String -> String -> IO ()
msg ed txt =
  do
    ed # state Normal
    appendText ed (txt ++ "\n")
    ed # state Disabled
    done

getMatchedFiles :: [FilePath] -> FilePath -> IO [FilePath]
getMatchedFiles fs abs = getMatchedFiles' fs [] abs
  where getMatchedFiles' :: [FilePath] -> [FilePath] -> FilePath ->
                            IO [FilePath]
        getMatchedFiles' (f : fs) dirs abs =
          do
            fstat <- getFileStatus (abs ++ f ++ "/")
            (if f == "." || f == ".." || not(isDirectory fstat) ||
                hidden f then
               getMatchedFiles' fs dirs abs
             else getMatchedFiles' fs (f : dirs) abs)
        getMatchedFiles' _ dirs _ = return dirs
        hidden :: FilePath -> Bool
        hidden ('.':_) = True
        hidden _ = False

toTreeListObjects :: TreeList String -> String -> [FilePath] ->
                     IO [TreeListObject String]
toTreeListObjects treelist path (f : fs) =
  do
    fstat <- getFileStatus (path ++ f ++ "/")
    acc <- system ("access -rx " ++ path)
    obj <- return (treelist, path ++ f ++ "/", f,
                   acc == ExitSuccess && isDirectory fstat)
    objs <- toTreeListObjects treelist path fs
    return (obj : objs)
toTreeListObjects _ _ _ = return []

cfun :: ChildrenFun String
cfun (tl, val, nm, isnode) =
  do
    putStrLn ("MAIN: cfun called for directory " ++ val ++ "\n")
    dcontents <- getDirectoryContents val
    matched_files <- getMatchedFiles dcontents val
    objs <- toTreeListObjects tl val matched_files
    return objs

ifun :: ImageFun String
ifun (_, val, _, _) = folderImg

launch :: Style -> () -> IO ()
launch st _ =
  do
    main <- newHFBox []
    box <- newVBox [parent main]
    win <- window main [text ("file browse example: " ++
                              if st == Pretty then "pretty style"
                              else "fast style")]
    treelist <- newTreeList st cfun ifun [parent box, background "white",
                                              size (cm 9, cm 10)]
    setRoot (treelist, "/", "/", True)
    show_hidden <- newCheckButton [text "show hidden files", parent box]
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent box, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    base64out <- newEditor [width 60, height 15,
                            state Disabled] :: IO (Editor String)
    base64_box <- newScrollBox base64out [parent main]
    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> msg base64out
                                      ((case mobj of
                                          Just (_, _, nm, _) -> nm
                                          _ -> "Nothing") ++ " selected")))
    sync (destroyed win)

main :: IO ()
main =
  do
    htk []
    main <- newVFBox []
    box <- newVBox [parent main]
    win <- window main [text "file browse example"]
    pretty <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                         text "pretty style", width 60,
                         command (launch Pretty)]
    fast <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "fast style", width 60, command (launch Fast)]
    quit <- newButton [pad Horizontal 10, pad Vertical 5, parent main,
                       text "Quit", width 60, command (\ () -> destroy win)]
    interactor (\i -> triggered pretty +> triggered fast +> triggered quit)
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

{- --------------------------------------------------------------------
 -
 - file browse example (as an example for the treelist module)
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

module Main (main) where

import HTk
import Image
import TreeList
import Button
import CheckButton
import Editor
import ScrollBox
import Maybe
import IO
import Directory
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
            putStrLn ("MAIN: getting permissions of " ++ f)
            p <- getPermissions (abs ++ f)
            putStrLn ("MAIN: got permissions\n")
            (if f == "." || f == ".." || not(searchable p) ||
                hidden f then
               getMatchedFiles' fs dirs abs
             else getMatchedFiles' fs (f : dirs) abs)
        getMatchedFiles' _ dirs _ = return dirs
        hidden :: FilePath -> Bool
        hidden ('.':_) = True
        hidden _ = False

notEmpty :: FilePath -> IO Bool
notEmpty fp =
  do
    putStr ("checking if '" ++ fp ++ "' is empty..");
    c <- getDirectoryContents fp
    putStrLn ("...ok")
    return (length c > 2)

toTreeListObjects :: TreeList String -> String -> [FilePath] ->
                     IO [TreeListObject String]
toTreeListObjects treelist path (f : fs) =
  do
    p <- getPermissions (path ++ f)
    acc <- system ("access -rx " ++ path)
    isnode <- if acc == ExitSuccess then notEmpty (path ++ f)
              else return False
--    isnode <- return (acc == ExitSuccess)
    obj <- return (treelist, path ++ f ++ "/", f, isnode)
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
    tk <- htk []
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
    destroy tk

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

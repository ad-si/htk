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
              r <- getMatchedFiles' fs dirs files abs
              return r
          else
            do
              b <- getPermissions (abs ++ "/" ++ f)
              (if not(hidden f)
               then if searchable b
                    then getMatchedFiles' fs (f : dirs) files abs
                    else if gif f
                         then getMatchedFiles' fs dirs (f : files) abs
                         else getMatchedFiles' fs dirs files abs
               else getMatchedFiles' fs dirs files abs)

        getMatchedFiles' _ dirs files _ = return (dirs ++ files)
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

toTreeListObjects :: TreeList String -> String -> [FilePath] ->
                     IO [TreeListObject String]
toTreeListObjects treelist path (f : fs) =
  do
    obj <- newTreeListObject treelist (path ++ "/" ++ dropQuotes(show f))
                             [name (dropQuotes (show f))]
    objs <- toTreeListObjects treelist path fs
    return (obj : objs)
toTreeListObjects _ _ _ = return []

cfun :: ChildrenFun String
cfun treelist obj =
  if gif (getObjValue obj) then
    return []
  else
    do
      dcontents <- getDirectoryContents (getObjValue obj)
      matched_files <- getMatchedFiles dcontents (getObjValue obj)
      objs <- toTreeListObjects treelist (getObjValue obj) matched_files
      return objs

ifun :: ImageFun String
ifun obj =
  do
    b <- getPermissions (getObjValue obj)
    if searchable b then folderImg else fileImg

main =
  do
    win <- htk []
    main <- newHFBox []
    box <- newVBox [parent main]
    win <- window main [text "Base64 encoding"]
    treelist <- newTreeList cfun ifun [parent box, background "white",
                                       size (cm 9, cm 10)]
    obj <- newTreeListObject treelist "/" [name "/"]
    setRoot treelist obj
    show_hidden <- newCheckButton [text "Show hidden files", parent box]
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent box, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    base64out <- newEditor [width 60, height 15, state Disabled] :: IO (Editor String)
    base64_box <- newScrollBox base64out [parent main]
    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> do
                                    objname <- case mobj of
                                                 Just obj -> getName obj
                                                 _ -> return "Nothing"
                                    msg base64out (objname ++ " selected")))
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

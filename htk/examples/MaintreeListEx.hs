{- ------------------------------------------------------------------------
 -
 - HTk Examples: Tree list example
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
import Editor
import ScrollBox
import Maybe

logMsg :: Editor String -> String -> IO ()
logMsg ed txt =
  do
    ed # state Normal
    appendText ed (txt ++ "\n")
    ed # state Disabled
    done

cfun :: ChildrenFun String
cfun treelist obj =
  case (getObjValue obj) of
      "/"              -> do
                            obj1 <- newTreeListObject treelist "/home"
                                                      [name "home"]
                            obj2 <- newTreeListObject treelist "/usr"
                                                      [name "usr"]
                            obj3 <- newTreeListObject treelist "/tmp"
                                                      [name "tmp"]
                            return [obj1, obj2, obj3]
      "/home"          -> do
                            obj1 <- newTreeListObject treelist "/home/ludi"
                                                      [name "ludi"]
                            return [obj1]
      "/usr"           -> do
                            obj1 <- newTreeListObject treelist "/usr/bin"
                                                      [name "bin"]
                            obj2 <- newTreeListObject treelist "/usr/lib"
                                                      [name "lib"]
                            obj3 <- newTreeListObject treelist "/usr/share"
                                                      [name "share"]
                            return [obj1, obj2, obj3]
      "/tmp"           -> return []
      "/home/ludi"     -> do
                            obj1 <- newTreeListObject treelist "/home/ludi/www"
                                                      [name "www"]
                            return [obj1]
      "/usr/bin"       -> return []
      "/usr/lib"       -> return []
      "/usr/share"     -> return []
      "/home/ludi/www" -> return []

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]

ifun :: ImageFun String
ifun obj =
  do
    img <- folderImg
    return img

main =
  do
    win <- htk []
    main <- newVFBox []
    box <- newHBox [parent main]
    win <- window main [text "Tree list example"]
    treelist <- newTreeList cfun ifun [parent box, background "white",
                                       size (cm 15, cm 10)]
    obj <- newTreeListObject treelist "/" [name "/"]
    setRoot treelist obj
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent box, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    output <- newEditor [width 40, height 6,
                         state Disabled] :: IO (Editor String)
    scrollbox <- newScrollBox output [parent main]
    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> do
                                    objname <- case mobj of
                                                 Just obj -> getName obj
                                                 _ -> return "Nothing"
                                    logMsg output (objname ++ " selected")))
    sync (destroyed win)

{- --------------------------------------------------------------------
 -
 - HTk Examples: Tree list example
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

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
cfun (tl, val, _, _) =
  case val of
      "/"              -> return [(tl, "/home", "home", True),
                                  (tl, "/usr", "usr", True),
                                  (tl, "/tmp", "tmp", True)]
      "/home"          -> return [(tl, "/home/ludi", "ludi", True)]
      "/usr"           -> return [(tl, "/usr/bin", "bin", True),
                                  (tl, "/usr/lib", "lib", True),
                                  (tl, "/usr/share", "share", True)]
      "/tmp"           -> return []
      "/home/ludi"     -> return [(tl, "/home/ludi/www", "www", True)]
      "/usr/bin"       -> return []
      "/usr/lib"       -> return []
      "/usr/share"     -> return []
      "/home/ludi/www" -> return []

ifun :: ImageFun String
ifun obj = folderImg

main =
  do
    win <- htk []
    main <- newVFBox []
    box <- newHBox [parent main]
    win <- window main [text "Tree list example"]
    treelist <- newTreeList Fast cfun ifun [parent box,
                                            background "white",
                                            size (cm 15, cm 10)]
    setRoot (treelist, "/", "/", True)
    quit <- newButton [side AtBottom,  pad Horizontal 10, pad Vertical 5,
                       parent box, text "Quit", width 15,
                       command (\ ()-> destroy win)]
    output <- newEditor [width 40, height 6,
                         state Disabled] :: IO (Editor String)
    scrollbox <- newScrollBox output [parent main]
    interactor (\i -> triggered quit +>
                      (selectionEvent treelist >>>=
                         \mobj -> logMsg output
                                         ((case mobj of
                                             Just (_, _, nm, _) -> nm
                                             _ -> "Nothing") ++
                                          " selected")))
    sync (destroyed win)

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]
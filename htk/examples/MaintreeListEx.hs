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
cfun obj =
  case getObjectID obj of
      "/" ->
        return [newTreeListObject "/home" "home" Node,
                newTreeListObject "/usr" "usr" Node,
                newTreeListObject "/tmp" "tmp" Leaf,
                newTreeListObject "/floppy" "floppy" Leaf,
                newTreeListObject "/opt" "opt" Node,
                newTreeListObject "/etc" "etc" Node]
      "/home" ->
        return [newTreeListObject "/home/ludi" "ludi" Node]
      "/usr" ->
        return [newTreeListObject "/usr/bin" "bin" Leaf,
                newTreeListObject "/usr/lib" "lib" Leaf,
                newTreeListObject "/usr/share" "share" Leaf]
      "/opt" ->
        return [newTreeListObject "/opt/Office51" "Office51" Leaf,
                newTreeListObject "/opt/fsuite" "fsuite" Leaf,
                newTreeListObject "/opt/gnome" "gnome" Leaf,
                newTreeListObject "/opt/kde" "kde" Leaf,
                newTreeListObject "/opt/netscape" "netscape" Leaf,
                newTreeListObject "/opt/nps" "nps" Leaf,
                newTreeListObject "/opt/oracle" "oracle" Leaf,
                newTreeListObject "/opt/skyrix" "skyrix" Leaf]
      "/etc" ->
        return [newTreeListObject "/etc/WindowMaker" "WindowMaker" Leaf,
                newTreeListObject "/etc/X11" "X11" Leaf,
                newTreeListObject "/etc/httpd" "httpd" Leaf,
                newTreeListObject "/etc/texmf" "texmf" Leaf,
                newTreeListObject "/etc/isdn" "isdn" Leaf,
                newTreeListObject "/etc/ssh" "ssh" Leaf]
      "/home/ludi" ->
        return [newTreeListObject "/home/ludi/www" "www" Leaf,
                newTreeListObject "/home/ludi/archiv" "archiv" Node]
      "/home/ludi/archiv" ->
        return [newTreeListObject "/home/ludi/archiv/download" "download" Leaf,
                newTreeListObject "/home/ludi/archiv/uni" "uni" Leaf,
                newTreeListObject "/home/ludi/archiv/haskell" "haskell" Leaf]

ifun :: ImageFun String
ifun obj = folderImg

main :: IO ()
main =
  do
    tk <- htk []
    main <- newVFBox []
    box <- newHBox [parent main]
    win <- window main [text "Tree list example"]
    treelist <- newTreeList Pretty cfun ifun (newTreeListObject "/" "/" Node)
                            [parent box, background "white",
                             size (cm 15, cm 8)]
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
                                             Just obj -> getObjectName obj
                                             _ -> "Nothing") ++
                                          " selected")) +>
                      (focusEvent treelist >>>=
                         \obj -> logMsg output (getObjectName obj ++
                                                " focused")))
    sync (destroyed win)
    destroy tk

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]
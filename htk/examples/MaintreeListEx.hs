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

module Main (main) where

import HTk
import TreeList
import ScrollBox

logMsg :: Editor String -> String -> IO ()
logMsg ed txt =
  do
    ed # state Normal
    appendText ed (txt ++ "\n")
    ed # state Disabled
    done

cfun :: ChildrenFun String
cfun obj =
  case getTreeListObjectValue obj of
      "/test" -> return [newTreeListObject "/test1" "test1" Leaf]

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
        return [newTreeListObject "/home/ludi/archiv/download" "download" 
                                  Leaf,
                newTreeListObject "/home/ludi/archiv/uni" "uni" Leaf,
                newTreeListObject "/home/ludi/archiv/haskell" "haskell"
                                  Leaf]
{-
ifun :: ImageFun String
ifun obj = folderImg
-}

main :: IO ()
main =
  do
    main <- initHTk [text "treelist example"]

    treelist <- newTreeList main cfun ifun
--                            (newTreeListObject "/" "/" Node)
                            [newTreeListObject "/" "/" Node,
                             newTreeListObject "/test" "test" Node,
                             newTreeListObject "/test2" "test2" Leaf]
                            [background "white", size (cm 15, cm 8)]
    pack treelist []

    quit <- newButton main [text "Quit", width 15] :: IO (Button String)
    pack quit [Side AtBottom, PadX 10, PadY 10]

    let out p = newEditor p [width 75, height 6,
                             state Disabled] :: IO (Editor String)

    (scrollbox, output) <- newScrollBox main out []
    pack scrollbox [PadX 10, PadY 10]

    clickedquit <- clicked quit
    spawnEvent (forever ((do
                            mobj <- receive (focusEvent treelist)
                            always (logMsg output
                                           ((case mobj of
                                               Just obj ->
                                                 getTreeListObjectName obj
                                               _ -> "no object") ++
                                            " focused"))) +>
                         (clickedquit >> always (destroy main)) +>
                         (do
                            mobj <- receive (selectionEvent treelist)
                            always (logMsg output
                                           ((case mobj of
                                               Just obj ->
                                                 getTreeListObjectName obj
                                               _ -> "Nothing") ++
                                            " selected")))))

    (htk_destr, _) <- bindSimple main Destroy
    sync (htk_destr)

folderImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7"]

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
import ReferenceVariables

cfun :: ChildrenFun String
cfun obj =
  case getTreeListObjectValue obj of
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

ifun :: ImageFun String
ifun obj = folderImg

main :: IO ()
main =
  do
    win <- initHTk [text "treelist"]
    tl <- newTreeList win cfun ifun (newTreeListObject "/" "/" Node)
                      [background "white", size (cm 8, cm 10)]
    pack tl []
    b <- newButton win [text "export state"] :: IO (Button String)
    pack b [Fill X]
    clickedb <- clicked b
    (htk_destr, _) <- bindSimple win Destroy
    spawnEvent (forever (clickedb >>>
                           do
                             st <- exportTreeListState tl
                             putStrLn "state exported"
                             t <- createToplevel [text "recovered state"]
                             tl' <- recoverTreeList t cfun ifun st
                                      [background "white",
                                       size (cm 5, cm 10)]
                             pack tl' []
                             putStrLn "state imported"))
    sync htk_destr

folderImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7"]

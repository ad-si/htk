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
    win1 <- initHTk [text "treelist1"]
    win2 <- createToplevel [text "treelist2"]

    tl1 <- newTreeList win1 cfun ifun [newTreeListObject "/" "/" Node]
                       [background "white", size (cm 8, cm 10)]
    pack tl1 []

    b1 <- newButton win1 [text "import other state"] :: IO (Button String)
    pack b1 [Fill X]
    clickedb1 <- clicked b1

    tl2 <- newTreeList win2 cfun ifun [newTreeListObject "/" "/" Node]
                       [background "white", size (cm 8, cm 10)]
    pack tl2 []

    b2 <- newButton win2 [text "import other state"] :: IO (Button String)
    pack b2 [Fill X]
    clickedb2 <- clicked b2

    (htk_destr, _) <- bindSimple win1 Destroy
    (win2_destr, _) <- bindSimple win2 Destroy

    spawnEvent ((win2_destr >>> destroy win1) +>
                (clickedb1 >>> do
                                 st <- exportTreeListState tl2
                                 putStrLn "state exported"
                                 importTreeListState tl1 st
                                 putStrLn "state imported") +>
                (clickedb2 >>> do
                                 st <- exportTreeListState tl1
                                 putStrLn "state exported"
                                 importTreeListState tl2 st
                                 putStrLn "state imported"))

    sync htk_destr

folderImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7"]

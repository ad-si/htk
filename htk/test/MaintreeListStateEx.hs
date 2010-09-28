
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.TreeList
import HTk.Toolkit.Name

data MyObj = MyObj String String (IO Image)

instance Eq MyObj where
  MyObj path1 _ _ == MyObj path2 _ _ = path1 == path2

instance CItem MyObj where
  getName (MyObj _ nm _) = return (createName nm)
  getIcon (MyObj _ _ img) = img

cfun :: ChildrenFun MyObj
cfun obj =
  let (MyObj path _ _) = getTreeListObjectValue obj in
  case path of
      "/" ->
        return [newTreeListObject (MyObj "/home" "home" folderImg) Node,
                newTreeListObject (MyObj "/usr" "usr" folderImg) Node,
                newTreeListObject (MyObj "/tmp" "tmp" folderImg) Leaf,
                newTreeListObject (MyObj "/floppy" "floppy" folderImg) Leaf,
                newTreeListObject (MyObj "/opt" "opt" folderImg) Node,
                newTreeListObject (MyObj "/etc" "etc" folderImg) Node]
      "/home" ->
        return [newTreeListObject (MyObj "/home/ludi" "ludi" folderImg) Node]
      "/usr" ->
        return [newTreeListObject (MyObj "/usr/bin" "bin" folderImg) Leaf,
                newTreeListObject (MyObj "/usr/lib" "lib" folderImg) Leaf,
                newTreeListObject (MyObj "/usr/share" "share" folderImg) Leaf]
      "/opt" ->
        return [newTreeListObject (MyObj "/opt/Office51" "Office51" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/fsuite" "fsuite" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/gnome" "gnome" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/kde" "kde" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/netscape" "netscape" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/nps" "nps" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/oracle" "oracle" folderImg) Leaf,
                newTreeListObject (MyObj "/opt/skyrix" "skyrix" folderImg) Leaf]
      "/etc" ->
        return [newTreeListObject (MyObj "/etc/WindowMaker" "WindowMaker" folderImg) Leaf,
                newTreeListObject (MyObj "/etc/X11" "X11" folderImg) Leaf,
                newTreeListObject (MyObj "/etc/httpd" "httpd" folderImg) Leaf,
                newTreeListObject (MyObj "/etc/texmf" "texmf" folderImg) Leaf,
                newTreeListObject (MyObj "/etc/isdn" "isdn" folderImg) Leaf,
                newTreeListObject (MyObj "/etc/ssh" "ssh" folderImg) Leaf]
      "/home/ludi" ->
        return [newTreeListObject (MyObj "/home/ludi/www" "www" folderImg) Leaf,
                newTreeListObject (MyObj "/home/ludi/archiv" "archiv" folderImg) Node]
      "/home/ludi/archiv" ->
        return [newTreeListObject (MyObj "/home/ludi/archiv/download" "download" folderImg)
                                  Leaf,
                newTreeListObject (MyObj "/home/ludi/archiv/uni" "uni" folderImg) Leaf,
                newTreeListObject (MyObj "/home/ludi/archiv/haskell" "haskell" folderImg)
                                  Leaf]

main :: IO ()
main =
  do
    main <- initHTk [text "treelist1"]
    win2 <- createToplevel [text "treelist2"]

    tl1 <- newTreeList main cfun [newTreeListObject (MyObj "/" "/" folderImg) Node]
                       [background "white", size (cm 8, cm 10)]
    pack tl1 []

    b1 <- newButton main [text "import other state"]
    pack b1 [Fill X]
    clickedb1 <- clicked b1

    tl2 <- newTreeList win2 cfun [newTreeListObject (MyObj "/" "/" folderImg) Node]
                       [background "white", size (cm 8, cm 10)]
    pack tl2 []

    b2 <- newButton win2 [text "import other state"]
    pack b2 [Fill X]
    clickedb2 <- clicked b2
    (win2_destr, _) <- bindSimple win2 Destroy

    spawnEvent (forever ((win2_destr >>> destroy main) +>
                         (clickedb1 >>> do
                                          st <- exportTreeListState tl2
                                          putStrLn "state exported"
                                          importTreeListState tl1 st
                                          putStrLn "state imported") +>
                         (clickedb2 >>> do
                                          st <- exportTreeListState tl1
                                          putStrLn "state exported"
                                          importTreeListState tl2 st
                                          putStrLn "state imported")))
    finishHTk

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ1UV+qjB659uWkBlj9tIBw873BQA7"]

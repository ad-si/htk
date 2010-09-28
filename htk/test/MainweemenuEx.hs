{- -----------------------------------------------------------------------
 -
 - HTk Examples: Menus
 -
 - Author: cxl
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module Main (main) where

import HTk.Toplevel.HTk

main :: IO ()
main =
  do
    main <- initHTk [text "Wee menus!", size(300, 300)]

    menubar <- createMenu main False []
    main # menu menubar

    v1 <- createTkVariable True
    c1 <- createMenuCheckButton menubar [text "MenuCheckButton", variable v1]
    val <- readTkVariable v1
    putStrLn (show val)
    clickedc1 <- clicked c1
    spawnEvent (forever (clickedc1 >>>do val <- readTkVariable v1
                                         putStrLn (show val)))
    (htk_destr, _) <- bindSimple main Destroy
    sync htk_destr

{- -----------------------------------------------------------------------
 -
 - HTk First Steps: Hello World with a button
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

module Main (main) where

import HTk

main :: IO ()
main =
  do main <- initHTk [text "My second HTk Program"]

     mb <- createMenu main False []
     main # menu mb

     pulldown1 <- createMenuCascade mb [text "File"]

     m <- createMenu mb False []
     pulldown1 # menu m
     qb <- createMenuCommand m [text "Quit"]

     l <- newLabel main [text "Hello, world!", relief Groove,
                         tooltip "this is a label widget"]
     pack l [Side AtBottom, PadX 10, PadY 10]

     clickedqb <- clicked qb
     spawnEvent (clickedqb >>> destroy main)

     (htk_destr, _) <- bindSimple main Destroy
     sync (htk_destr)

{- this does not return when the main window is destroyed!
--  simpler:
     sync clickedqb
     destroy main
-}
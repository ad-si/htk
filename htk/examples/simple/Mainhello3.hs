
module Main (main) where

import HTk.Toplevel.HTk

main :: IO ()
main =
  do main <- initHTk [text "My third HTk Program"]

     mb <- createMenu main False []
     main # menu mb

     pulldown1 <- createMenuCascade mb [text "File"]

     m <- createMenu mb False []
     pulldown1 # menu m
     qb <- createMenuCommand m [text "Quit"]

     f  <- newFrame main []
     f2 <- newFrame f []
     v1 <- createTkVariable ""
     l  <- newLabel f [text "Rename: "]
     e1 <- (newEntry f2 [variable v1])::IO (Entry String)
     e2 <- (newEntry f2 [variable v1])::IO (Entry String)

     (entered, _) <-
       bind e1 [WishEvent [] (KeyPress (Just (KeySym "Return")))]

     pack f []
     pack l [PadX 10, Side AtLeft]
     pack f2 [Side AtRight]
     pack e1 [PadX 10, Side AtTop]
     pack e2 [PadX 10, Side AtBottom]

     clickedqb <- clicked qb
     spawnEvent (forever ((clickedqb >>> destroy main) +>
                          (entered >>> do txt <- readTkVariable v1
                                          main # text txt >> done)))

     finishHTk

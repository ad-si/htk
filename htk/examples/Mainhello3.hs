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

main :: IO ()
main = 
  do main <- initHTk [text "My third HTk Program"]

     mb <- createMenu main False []
     main # menu mb

     pulldown1 <- createMenuCascade mb [text "File"]

     m <- createMenu mb False []
     pulldown1 # menu m
     qb <- createMenuCommand m [text "Quit"]

     f <- newFrame main []
     v1 <- createTkVariable ""
     l <- newLabel f [text "Rename: "]
     e <- (newEntry f [variable v1])::IO (Entry String)
	
     pack f []
     pack l [PadX 10, Side AtLeft]
     pack e [PadX 10, Side AtRight]

     clickedqb <- clicked qb
     (entered, _) <-
       bind e [WishEvent [] (KeyPress (Just (KeySym "Return")))]
     spawnEvent (forever ((clickedqb >>> destroy main) +>
                          (entered >>> do txt <- readTkVariable v1
				          main # text txt >> done)))

     finishHTk
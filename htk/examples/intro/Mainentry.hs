module Main where

import HTk.Toplevel.HTk

main :: IO ()

main =
  do main <- initHTk [text "Entry example"]

     f <- newFrame main []
     l <- newLabel f [text "Rename: "]
     e <- (newEntry f [value "", width 20])::IO (Entry String)

     (entered, _) <-
       bind e [WishEvent [] (KeyPress (Just (KeySym "Return")))]

     pack f []
     pack l [PadX 10, Side AtLeft]
     pack e [PadX 10, Side AtRight]

     spawnEvent
      (forever
        (entered >>> do txt <- (getValue e) :: IO String
                        e # value ""
                        main # text txt >> done))

     finishHTk


module Main where

import HTk.Toplevel.HTk
import Random

main:: IO ()
main =
  do main <- initHTk []

     b <- newButton main [text "Press me!"]
     b2 <- newButton main [text "Close"]
     pack b [Side AtTop, Fill X]
     pack b2 [Side AtBottom, Fill X]

     click  <- clicked b
     click2 <- clicked b2
     spawnEvent
      (forever
        ((click >>> do nu_label <- mapM randomRIO
                                       (replicate 5 ('a','z'))
                       b # text nu_label
                       done)
        +> (click2 >>> destroy main)))
     finishHTk


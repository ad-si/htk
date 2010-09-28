module Main where

import HTk.Toplevel.HTk
import Random

main:: IO ()
main =
  do main <- initHTk []

     b <- newButton main [text "Press me!"]
     pack b []

     click <- clicked b
     spawnEvent
      (forever
        (click >>> do nu_label <- mapM randomRIO
                                       (replicate 5 ('a','z'))
                      b # text nu_label))
     finishHTk


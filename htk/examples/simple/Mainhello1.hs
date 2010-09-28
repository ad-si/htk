
module Main (main) where

import HTk.Toplevel.HTk
import Random(randomRIO)

randomColour :: IO (Int, Int, Int)
randomColour = do
   red  <-randomRIO(0,255)
   green<-randomRIO(0,255)
   blue <-randomRIO(0,255)
   return (red, green, blue)

main:: IO ()
main =
  do
    main <- initHTk []

    Icon main # bitmap info

    l  <- newLabel main [height 5, text "Hello, world!"]
    pack l []

    nb <- newButton main [text "New Colour"]
    pack nb []
    clickednb <- clicked nb
    spawnEvent (forever (clickednb >>>
                         do bunt <- randomColour
                            l # foreground bunt))
    finishHTk

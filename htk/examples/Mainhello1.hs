{- -----------------------------------------------------------------------
 -
 - HTk First steps: the ubiquitous "Hello World!"
 -
 - Author: cxl 
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

module Main (main) where

import HTk
import Random(randomRIO)

randomColour :: IO (Int, Int, Int)
randomColour = do
   red  <-randomRIO(0,255)
   green<-randomRIO(0,255)
   blue <-randomRIO(0,255)
   return (red, green, blue)
      
main:: IO ()
main =
  do main <- initHTk []

     l  <- newLabel main [text "Hello, world!"]
     pack l []

     nb <- newButton main [text "New Colour"] :: IO (Button String)
     pack nb []
     clickednb <- clicked nb
     spawnEvent (forever (clickednb >>>
                          do bunt <- randomColour
                             l # foreground bunt))

     (htk_destr, _) <- bindSimple main Destroy
     sync (htk_destr)



module Main (main) where

import HTk
import Random(randomRIO)
import Concurrent(threadDelay)

toInt :: Distance -> Int
toInt = fromInteger . toInteger

randomColour :: IO (Int, Int, Int)
randomColour =
  do
    red  <-randomRIO(0,255)
    green<-randomRIO(0,255)
    blue <-randomRIO(0,255)
    return (red, green, blue)

nextColour :: (Int, Int, Int)-> IO (Int, Int, Int)
nextColour (r, g, b) =
  do
    red  <-randomRIO(0,10)
    green<-randomRIO(0,10)
    blue <-randomRIO(0,10)
    return (mk (r+red), mk (g+green), mk (b+blue))
  where mk col = max (col `mod` 255) 25

main :: IO ()
main =
  do
    main <- initHTk [text "Pretty Blobs"]
    cnv <- newCanvas main [size (cm 15, cm 15),
		           background "black"]
    pack cnv []

    (press, _) <- bind cnv [WishEvent [] (ButtonPress (Just 1))]
    spawnEvent (forever (do
                           (x, y) <- press >>>= \i-> return (x i, y i)
                           always (do
                                     col <- randomColour
			             c <- colourDot cnv x y col
			             spawn (sparkle c (x,y) col 0 255))))
    finishHTk

   where colourDot cnv x y col = createOval cnv [filling col, size (2, 2),
                                                 position (x - 1, y - 1)]
	 sparkle p (x,y) col cnt fade =
	   if cnt >= 750 then do destroy p  -- doesn't remove image ?!? 
	   else do
                  p # filling (col)
	          p # size (cnt `div` 5,  cnt `div` 5)
		  p # position (x- cnt `div` 10, y- cnt `div` 10)
		  col <- nextColour col >>= return . fadeColour fade
		  threadDelay 20
	          sparkle p (x, y) col (cnt+10) 
	  	          (if cnt >= 500 then fade - 10 else fade)
	 fadeColour f (r, g, b) = (min r f, min g f, min b f)

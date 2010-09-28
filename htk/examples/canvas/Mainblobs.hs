

module Main (main) where

import HTk.Toplevel.HTk
import Random(randomRIO)
import Control.Concurrent(threadDelay)

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
    wmain <- initHTk [text "Pretty Blobs"]
    cnv <- newCanvas wmain [size (cm 15, cm 15),
                           background "black"]
    pack cnv []

    (press, _) <- bind cnv [WishEvent [] (ButtonPress (Just 1))]
    spawnEvent (forever (do
                           (nx, ny) <- press >>>= \i-> return (x i, y i)
                           always (do
                                     col <- randomColour
                                     c <- colourDot cnv nx ny col
                                     spawn (sparkle c (nx, ny) col 0 255))))
    finishHTk


colourDot :: Canvas -> Distance -> Distance -> (Int, Int, Int) -> IO Oval
colourDot cnv nx ny col = createOval cnv [filling col, size (2, 2),
                                                 position (nx - 1, ny - 1)]

sparkle :: Oval -> (Distance, Distance) -> (Int, Int, Int) -> Distance
        -> Int -> IO ()
sparkle p dp@(nx, ny) col cnt fade =
           if cnt >= 750 then do destroy p  -- doesn't remove image ?!?
           else do
                  p # filling (col)
                  p # size (cnt `div` 5,  cnt `div` 5)
                  p # position (nx - cnt `div` 10, ny - cnt `div` 10)
                  ncol <- nextColour col >>= return . fadeColour fade
                  threadDelay 20
                  sparkle p dp ncol (cnt + 10)
                          (if cnt >= 500 then fade - 10 else fade)

fadeColour :: Int -> (Int, Int, Int) -> (Int, Int, Int)
fadeColour f (r, g, b) = (min r f, min g f, min b f)

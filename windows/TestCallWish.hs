module Main where

import CString
import Concurrent

import CallWish


-- Should check that GHC isn't stopping the world.
clock n =
   do
      threadDelay 1000000
      putStrLn (show n)
      clock (mod (n+1) 60)

main :: IO ()
main =
   do
      calledWish <- callWish
      cmd <- newCStringLen 
         "button .hello -text Hello -command {puts Hello};pack .hello\n"
      sendCalledWish calledWish cmd
      line1 <- getLine
      line2 <- getLine
      let
         monitor = 
            do
               str <- readCalledWish calledWish
               putStrLn str
               monitor
      forkIO monitor
      clock 0


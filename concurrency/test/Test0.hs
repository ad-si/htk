module Main (
        main 
        ) 
where

import Concurrency
import Dynamics

import qualified IO
import Debug(debug)

main :: IO ()
main =
   do 
      forkIOnull(test1)
      delay 1000000000
      done

test1 = 
   do 
     ch2 <- newChannel;
     forkIOnull (producer ch2);
     forever (
        do
           display "Receiving" 
           sync (
            receive ch2 >>> display "."
            )
        )

producer ch2 = 
   forever (
      do
         display "Sending"
         sync (send ch2 ())
      )
display s = IO.putStr s >> IO.hFlush IO.stdout

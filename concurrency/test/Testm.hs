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
      ch <- newChannel 
      forkIOnull(testm)
      sync (receive ch)      
      done

testm = forever (display "A")

display s = IO.putStr s >> IO.hFlush IO.stdout

{- This is a test for the Notification module.  
   Thus several instances of this test need to run, preferably
   on different machines, at the same time as the server in 
   uni/concurrency/server.
   
   This program should have 3 arguments:
   (1) The name of the machine on which the server is running.
   (2) A starting number (an Int)
   (3) A stopping number (an Int)
   -}
module Main(main) where

import System

import Computation(done)
import Thread(forkIO)
import Selective(deadlock)
import SIM
import Notification

main :: IO ()
main =
   do
      [host,startingNo,stoppingNo] <- getArgs
      let 
         start = read startingNo :: Int
         stop = read stoppingNo :: Int
      notifier <- mkNotifier host
      forkIO(action notifier start stop)
      quitMessage <- getLine
      done
      

action :: Notifier -> Int -> Int -> IO ()
action notifier loop stop =
   if loop == stop
      then
         do
            report "Finished"
            shutdown -- and quit
      else
         do
            if (loop /= 0) 
               then
                  do
                     let
                        thisKey = show loop
                     sync(isNotified notifier thisKey)
                     report ("Heard "++thisKey)
               else
                  done
            let next = show (loop+1)
            report ("Notifying "++ next)
            notify notifier next
            action notifier (loop+2) stop -- and recurse       

report :: String -> IO ()
report str = putStr (str ++ "\n")


module Main where

import CString
import Concurrent

import CallWish

main :: IO ()
main =
   do
      calledWish <- callWish
      let
         monitorOutput =
            do
               nextLine <- readCalledWish calledWish
               putStrLn (show nextLine)
               monitorOutput
         getInput =
            do
               nextLine <- getLine
               if nextLine == "stop" 
                  then
                     destroyCalledWish
                  else
                     do
                        withCStringLen (nextLine ++ "\n") 
                           (sendCalledWish calledWish)
                        getInput
      forkIO monitorOutput
      getInput 
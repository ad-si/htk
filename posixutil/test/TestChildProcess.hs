module Main where

import Control.Concurrent

import Posixutil.ChildProcess

runProg :: String -> IO ()
runProg progName =
   do
      child <- newChildProcess progName []
      forkIO (passOutput child)
      passInput child

passInput :: ChildProcess -> IO ()
passInput child =
   do
      input <- getLine
      sendMsg child input
      passInput child

passOutput :: ChildProcess -> IO ()
passOutput child =
   do
      output <- readMsg child
      putStrLn ("Got: "++output)
      passOutput child

main = runProg "wish"

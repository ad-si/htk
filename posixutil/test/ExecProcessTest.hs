{- Function which runs a program, given as its argument, passing stdin to it,
   and printing the program's stdout. -}
module ExecProcessTest where

import Concurrent

import Posix

runProg :: String -> IO ()
runProg progName =
   do
      (readIn,writeIn) <- Posix.createPipe 
      (readOut,writeOut) <- Posix.createPipe 

      processID <- forkProcess
      case processID of
         Nothing -> 
            -- Child process
            do
                dupTo readIn stdInput
                dupTo writeOut stdOutput
                Posix.executeFile progName True [] Nothing
         Just _ ->
            -- Parent process
            do
               forkIO (passOutput readOut)
               passInput writeIn

passInput :: Fd -> IO ()
passInput fd =
   do
      input <- getLine
      fdWrite fd (input ++ "\n")
      passInput fd

passOutput :: Fd -> IO ()
passOutput fd =
   do
      threadWaitRead (fdToInt fd)
      (output,_) <- fdRead fd 1000
      putStrLn ("Got: "++output)
      passOutput fd
{- SafeSystem.safeSystem executes a command (supplied as a String) and
   returns its exit code.  It differs from System.system in that it does
   NOT stop the world while doing this, so that other threads can run.
   How it works: we use ChildProcess to run the runCommand C program,
   and feed it the command over stdin.  Ugly, but is there a better way?
   -}
module SafeSystem(
   safeSystem
   ) where

import System

import WBFiles
import FileNames

import ChildProcess

safeSystem :: String -> IO ExitCode
safeSystem command =
   do
      -- Get location of runCommand
      top <- getTOP
      let 
         fullName = (trimDir top) `combineNames`
            ("posixutil" `combineNames` "runCommand")
      childProcess <- newChildProcess fullName [
         linemode True,
         standarderrors True
         ]
      sendMsg childProcess (command++"\n")
      let
         readOutput =
            do
               let
                  -- we ignore blank input lines.
                  notExit "" = readOutput
                  notExit str =
                     do
                        putStrLn ("SafeSystem: "++command++": "++str)
                        readOutput
               nextLine <- readMsg childProcess
               case nextLine of
                  'E':'X':'I':'T':'C':'O':'D':'E':' ':numberStr ->
                     case readsPrec 0 numberStr of
                        [(0,"")] -> return ExitSuccess
                        [(n,"")] -> return (ExitFailure n)
                        _ -> notExit nextLine
                  _ -> notExit nextLine 
      readOutput                     
                  

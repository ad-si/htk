-- | SafeSystem.safeSystem executes a command (supplied as a String) and
-- returns its exit code.  It differs from System.system in that it does
-- NOT stop the world while doing this, so that other threads can run.
-- How it works: we use ChildProcess to run the runCommand C program,
-- and feed it the command over stdin.  Ugly, but is there a better way?
module Posixutil.SafeSystem(
   safeSystemGeneral,
   safeSystem,
   ) where

import System.Exit

import Util.WBFiles
import Util.FileNames
import Util.Computation

import Posixutil.ChildProcess

safeSystem :: String -> IO ExitCode
safeSystem command =
   let
      -- We ignore blank output lines.
      outputSink "" = done
      outputSink str = putStrLn ("SafeSystem output: "++str)
   in
      safeSystemGeneral command outputSink

-- | Run \"command\", displaying any output using the supplied
-- outputSink function.  (This output had better not include
-- \"EXITCODE [number]\".)
--
-- outputSink is fed output line by line, and without the newlines.
safeSystemGeneral :: String -> (String -> IO ()) -> IO ExitCode
safeSystemGeneral command outputSink =
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
                  notExit str =
                     do
                        outputSink str
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


{- This program dumps the server files (the locs file and the version info 
   file) to its stdout.  -}
module Main where

import WBFiles

import LogFile
import VersionInfo
import SimpleDBServer

main =
   do
      let
         sList :: Show a => [a] -> String
         sList = unlines . (map show)

      (versionInfos :: [VersionInfo]) <- readLog "versionInfos"
      putStrLn ("Version Infos:\n" 
         ++ sList versionInfos)

      (serverOps :: [ServerOp]) <- readLog "objectLocs"
      putStrLn ("\n\n--------------------\nServer Ops\n" 
         ++ sList serverOps)
   
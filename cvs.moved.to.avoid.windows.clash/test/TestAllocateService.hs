{- TestAllocateService tests allocation via the allocate service.
   There should be 1 argument, the host on which the allocate service
   is running.  We assume a port of 11393.

   The tests you can run are (a) enter a blank line, which gives
   you a new CVS file; (b) enter two non-empty strings,
   which finds a new version; the first string should be a file and
   the second the old version.
   -}
module Main(main) where


import System

import IOExtras

import CallServer

import CVSTypes
import AllocateService

main =
   do
      (queryFn,disconnect,header) <- connectReply allocateService
      putStrLn ("Connected "++header)
      catchEOF (doQueries queryFn)
      disconnect

doQueries :: (AllocateRequest -> IO AllocateAnswer) -> IO ()
doQueries queryFn =
   do
      fileOpt <- getLine
      if (fileOpt == "") 
         then
            do
               result <- queryFn GetNewCVSFile
               putStrLn (show result)
               doQueries queryFn
         else
            do
               version <- getLine
               result <- queryFn 
                  (GetNewCVSVersion (CVSFile fileOpt) (CVSVersion version))
               putStrLn (show result)
               doQueries queryFn
               




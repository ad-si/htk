-- Test what happens when you threadWaitRead on a pipe which is closed.
-- See comment in reactor/ChildProcess.hs.
-- Compile with (on this system)
--    /usr/local/pub-bkb/ghc/ghc-2.10/bin/ghc -syslib posix -concurrent -cpp ThreadWaitFail.hs -DOLD_VERSION -fglasgow-exts -o TWF 
-- (old version) and
--    ghc -syslib posix -syslib concurrent -cpp ThreadWaitFail.hs -o TWF
-- (current version)

module Main(main) where

import Posix
import PosixUtil
import Concurrent

#ifdef OLD_VERSION
import GlaExts

fdToInt :: Fd -> Int
fdToInt (FD# fd#) = I# fd# 
#endif

main =
   do
      (read,write) <- Posix.createPipe
#ifdef REALFORK
      mpid <- Posix.forkProcess
      case mpid of
         Just child -> -- parent process
            do
               fdClose read
               putStr "hello, world!"
               response <- getLine
               putStr response
         Nothing ->
            threadWaitRead(fdToInt read)
#else
      forkIO(threadWaitRead(fdToInt read))
      fdClose read
      putStr "hello, world!"
      response <- getLine
      putStr response
#endif       



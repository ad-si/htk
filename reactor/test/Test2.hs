module Main (
        main    
        ) where

import Concurrency(forkIO,block)
import ThreadWait
import qualified Posix
import PosixUtil

import ChildProcess
import Debug(debug)

main = do {
        wish <- newChildProcess "wish" [];
        sendMsg wish "pack [.w0 entry -bg white]\n";
        forkIO (reader wish);
        block
        }

reader wish = do {
        catch (do {str <- readMsg wish; print str}) print;
        reader wish
        }

{-
main =  do {
        forkIO reader;
        threadDelay (secs 2);
        print "Closing stdin";
        Posix.fdClose (intToFd (0::Int));
}

reader = threadWaitRead 0

msecs i = (1000 * i)
secs i   = 1000 * (msecs i)

-}

msecs i = (1000 * i)
secs i   = 1000 * (msecs i)


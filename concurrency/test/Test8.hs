module Main (
        main 
        ) 
where

import Posix (exitImmediately)
import Concurrency
import System
import Mutex
import Variable
import Debug(debug)

main :: IO ()
main = do {
        tid <- getThreadID;
        pv <- newPVar tid;
        l <- newMutex;
        forkIO (counter l pv); 
        forkIO (counter l pv);
        block
        }

counter :: Mutex -> PVar ThreadID -> IO ()
counter l pv = do {
        tid <- getThreadID;
        synchronize l (do {
                setVar pv tid;
                synchronize l (putStr ("Holder = " ++ {- show tid -} "???" ++ "\n"));
                val <- getVar pv;
                when (tid /= val) (do {
                        print "Test Failed";
                        exitImmediately ExitSuccess
                        })
                });
        counter l pv;
}

module Main (
        main 
        ) 
where

import Posix (exitImmediately)
import Concurrency
import System
import RVar
import Debug(debug)

main :: IO ()
main = do {
        x <- newRVar 0;
        forkIO (counter x 1); 
        forkIO (counter x 2);
        block
        }

counter :: RVar Int -> Int -> IO ()
counter x n = do {
        tid <- getThreadID;
        synchronize x (do {
                setVar x n;
                putStr ("thread id = " ++ {- show tid -} "??" ++ "\n");
                val <- getVar x;
                putStr ("x = " ++ show val ++ "\n");
                when (n /= val) (exitImmediately ExitSuccess);
                done
                });
        counter x n;
}

module Main (
        main 
        ) 
where

import Concurrency
import Dynamics
import Debug(debug)

main :: IO ()
main = do {
        tid <- getThreadID;
        testPersistence tid
} where testPersistence tid = do {
                tid' <- getThreadID;
                when (tid /= tid') (error "Thread ID is not persistent");
                testPersistence tid             
                }

module Main (
        main 
        ) 
where

import Concurrency

-- import qualified Actor
import RVar
import Mutex
import Queue
import FiniteMap
import Maybes
import Debug(debug)

main :: IO ()
main = try (script) >> done

script2 = sync (inaction :: EV ()) >> putStr "done\n"

script = {-
        newChannel                                              >>= \ ch ->
        forkIO(reader ch)                                               >>
        forkIO(writer ch "ABC")                                 >> 
        consumer                                                >>= \ aid1 ->
        forkIO (producer (1 :: Int) aid1)                       >>
-}
        newRVar 1                                               >>= \ rv ->
        try(forkIO(testRVar rv))                                        >>
        try(forkIO(testRVar rv))                                        >>
        try(forkIO(testRVar rv))                                        >>
        done

reader ch = forever ( try (sync (scr)) >> delay (secs 1))
        where scr =
                receive ch >>>= \val -> 
                print (show val)                        >>
                deadlock

writer ch val = forever (sendIO ch val >> delay (secs 2))
        

testRVar rv = do {
        tid <- getThreadID;
        forever(updVar rv (f tid rv));
        done    
} where f :: ThreadID -> RVar Int -> Int -> IO (Int,Int)
        f tid rv val = do {
                t <- getThreadID;
                unless (t == tid) (error "Illegal thread in critical region");
--                print ("Thread = " ++ show t);
                print ("VAL = " ++ show val);
                if val == 10 then do {
                        print (show 10);
                        return (1,1)
                        }
                else do {
                        setVar rv (val +1);
                        val' <- updVar rv (f tid rv);
                        return (val',val')
                }
        }


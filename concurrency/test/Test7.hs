module Main (
        main 
        ) 
where

import Concurrency
import Debug(debug)

main :: IO ()
main = do {
        q1 <- newMsgQueue;
        q2 <- newMsgQueue;
        forkIO (producer count q1 q2);
        consumer count q1 q2
} where cno = 2
        count = 100000

        producer 0 _ _ = print "producer done"
        producer n q1 q2 = do {
                sendIO q1 n; 
                producer (n-1) q2 q1
                }

        consumer 0 _ _ = print "consumer done"
        consumer n q1 q2 = do {
                n' <- sync (receive q1 +> receive q2); 
                print n'; 
                consumer (n-1) q1 q2
                }


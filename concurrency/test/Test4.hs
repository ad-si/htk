module Main (
        main 
        ) 
where

import Concurrency
import Debug(debug)

main :: IO ()
main =  test


test = do {
        ch <- newMsgQueue;
        forkIO(producer ch count);
        consumer ch 0;
        test
} where count = 100
        consumer ch s = do {
                n <- sync(receive ch); 
                if n > 1 then do {
                        print n;
                        consumer ch (s + n)
                        }
                else do {
                        print "1 (and done)";
                        unless (s == (foldr1 (+) [2..count])) 
                                (error "test failed")
                        }
                }
        producer ch 0 = done
        producer ch n = do {sendIO ch n; producer ch (n-1)}

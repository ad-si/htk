module Main (
        main 
        ) 
where

import Concurrency
import qualified Concurrent
import Debug(debug)

main :: IO ()
main = do {
        ch <- newChannel;
        forkIO(consumer ch count);
        producer ch count;
} 
  where count = 100000
        producer ch 0 = print "producer done"
        producer ch n = do {
                sendIO ch n; 
                producer ch (n-1)
                }
        consumer ch 0 = print "consumer done"
        consumer ch ne = do {
                Concurrent.yield;
                n <- receiveIO ch;
                when (ne /= n) (error "message lost");
                consumer ch (ne-1)
                }

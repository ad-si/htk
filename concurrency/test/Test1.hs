module Main (
        main 
        ) 
where

import Concurrency
import Dynamics
import Debug(debug)

main :: IO ()
main = do {
        forkIOnull(test1);
--      forkIOnull(test2);
--      forkIOnull(test3);
--      forkIOnull(test4);
--      forkIOnull(test5);
        block;
        }


test1 = do {
        ch1 <- newChannel;
        ch2 <- newChannel;
        forkIOnull (producer ch2);
        forever ( sync (
      {-          receive ch1 >>> error "self communication on receive"
          +>    send ch1 () >>> error "self communication on send"
          +> -}    receive ch2 >>> putStr "."
        ))
}


producer ch2 = forever (sync (send ch2 ()))


test2 = do {
        ch1 <- newChannel;
        ch2 <- newChannel;
        forkIOnull (communicator ch1 ch2 "A");
        forkIOnull (communicator ch2 ch1 "B");
        done
} where communicator ch1 ch2 s = forever (sync (
                receive ch1 >>>= putStr
             +> send ch2 s >>> done
            ))


test3 = do {
        ch0 <- newChannel;
        ch1 <- newChannel;
        forkIOnull (producer ch1 ch0 1);
        ch2 <- newChannel;
        forkIOnull (producer ch2 ch0 2);
        ch3 <- newChannel;
        forkIOnull (producer ch3 ch0 3);
        forkIOnull (reader ch0 ch1 ch2 ch3 0 0 0 0)
} where count = 50000
        producer ch ch0 no = do { 
                doTimes count (sendIO ch no);
                sendIO ch0 0
                }
        reader ch0 ch1 ch2 ch3 ones twos threes 3 = do {
                if (ones == count) && (twos == count) && (threes == count) then
                        putStr "\nTEST 3 SUCCEEDED\n"
                else
                        error "counting error by communication"
                }
        reader ch0 ch1 ch2 ch3 ones twos threes zeros = sync (
                receive ch0 >>> 
                        reader ch0 ch1 ch2 ch3 ones twos threes (incr zeros)
             +> receive ch1 >>> 
                        reader ch0 ch1 ch2 ch3 (incr ones) twos threes zeros
             +> receive ch2 >>> 
                        reader ch0 ch1 ch2 ch3 ones (incr twos) threes zeros
             +> receive ch3 >>> 
                        reader ch0 ch1 ch2 ch3 ones twos (incr threes) zeros
            )



-- doTimes n c = sequence (take n (repeat c))

doTimes 0 c = done
doTimes n c = do {c;doTimes (n - 1) c}

incr x = x + 1




test4 = do {
        tid <- getThreadID;
        testPersistence tid
} where testPersistence tid = do {
                delay (secs 0.1);
                tid' <- getThreadID;
                when (tid /= tid') (error "Thread ID is not persistent");
                testPersistence tid             
                }


test5 = do {
        delay (secs 0.1);
        forkIOnull test5;
        done
        }

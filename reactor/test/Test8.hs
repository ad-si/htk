module Main (
        main 
        ) 
where

import SIM
import Object
import Dynamics
import EventBroker
import qualified Concurrency(forkIOnull,block)
import Debug(debug)

main :: IO ()
main =  do {
        adp <- newEventBroker;
        iact <- interactor (const (
                choose [ (listenA adp n >>> sync(listenA adp n)) | n <- [1..cno]]));
        Concurrency.forkIOnull (producer 1 adp);
        Concurrency.block
} where cno = 50

        producer :: Int -> EventBroker String -> IO ()
        producer n adp = do {
                dispatch adp (1::Int) (show n) done;
                delay (secs 0.005);
                producer (n+1) adp
                }


instance EventDesignator Int where
        toEventID n = EventID (ObjectID n) (show n)


listenA :: EventBroker String -> Int -> IA ()
listenA adp n = awaitEvent adp n Notice done done >>>= print

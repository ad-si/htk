module Main (
        main 
        ) 
where

import SIM
import Object
import Dynamics
import EventBroker
import IO(stdout)
import qualified Concurrency(forkIOnull,block)
import Debug(debug)

main :: IO ()
main =  do {
        setLogFile (Just stdout);
        adp <- newEventBroker;
        iact <- interactor (\iact ->
                choose [ (listenA adp n >>> stop iact) | n <- [1..1]]);
        Concurrency.forkIOnull (producer 1 adp);
        Concurrency.block
} where cno = 50

        producer :: Int -> EventBroker String -> IO ()
        producer n adp = do {
                dispatch adp (1::Int) (show n) done;
                delay (secs 1);
                dispatch adp (1::Int) (show n) done;
                delay (secs 1);
                dispatch adp (1::Int) (show n) done;
                }


instance EventDesignator Int where
        toEventID n = EventID (ObjectID n) (show n)


listenA :: EventBroker String -> Int -> IA ()
listenA adp n = awaitEvent adp n Notice done done >>>= print

{- #########################################################################

MODULE        : Main
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Main (
        main    
        ) where


import SIM
import Object

import ChildProcess
import EventBroker
import Concurrency
import Debug(debug)

main =  do {
        brk <- newEventBroker;
        listener brk;
        listener brk;
        forkIO (generator brk (1::Int));
        forkIO (generator brk (2::Int));
        delay(secs 20);
        generator brk (3::Int);
        block;
        done
} where reg = do {print "registering event"; done}
        unreg = do {print "unregistering event"; done}
        listener brk = 
                interactor (\iact -> 
                        awaitEvent brk (1 :: Int) Request reg unreg >>>
                                print "caught 1"
                  +>    awaitEvent brk (2 :: Int) Notice reg unreg >>>
                                print "caught 2"
                  +>    awaitEvent brk (3 :: Int) Notice reg unreg >>> do {
                                print "caught 3"; stop iact}
                );

generator brk sig = do {
        delay (secs 1);
        putStr ("generating " ++ show sig ++ "\n");
        dispatch brk sig () done;
        generator brk sig
}

instance EventDesignator Int where
        toEventID n = EventID (ObjectID n) (show n)

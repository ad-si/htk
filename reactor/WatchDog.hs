{- #########################################################################

MODULE        : WatchDog
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Polling Sensor that send a signal when a certain 
                condition has occured. 

                Being a true watchdog: once triggered, it wont stop.


   ######################################################################### -}


module WatchDog (
        HasReceiveEV(..),
        HasReceiveIO(..),
        Destructible(..),

        WatchDog,
        newWatchdog

        ) where

import Concurrency
import Interaction
import Future
import SIMClasses(Destructible(..))
import Debug(debug)


-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data WatchDog a =  WatchDog (Future a) (MVar (Bool,EV ()))


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance HasReceiveEV WatchDog a where
        receive (WatchDog ft _) = receive ft

instance Destructible (WatchDog a) where
        destroy (WatchDog ft mv) = setVar mv (False,always ())
        destroyed (WatchDog ft mv) = event (getVar mv >>= return . snd) |>> done


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newWatchdog :: Duration -> IO (Maybe a) -> IO (WatchDog a)
newWatchdog time cmd = do {
        mv <- newMVar (True,inaction);
        ft <- newFuture (sensor cmd mv);
        return (WatchDog ft mv)
} where sensor cmd mv = do {
                st <- cmd;
                (go,_) <- getVar mv;
                unless go deadlock;
                case st of
                        Nothing -> delay time >> sensor cmd mv
                        (Just a) -> return a
                }

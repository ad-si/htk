{- #########################################################################

MODULE        : WatchDog
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Polling Sensor that send a signal when a certain 
                condition has occured.  (You set one of these up
                each time you are waiting for something to happen.)

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
-- The Future (part of the concurrency toolkit) is the event
-- (or computation or whatever) we are waiting to complete.
-- Once completed the value can be retrieved again and again via
-- the Watchdog's receive event.
--
-- The Bool is initially True and becomes False when the WatchDog
-- is destroyed.
-- The EV() is either inaction before the event is destroyed,
-- or "always()" afterwards.

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance HasReceiveEV WatchDog a where
   receive (WatchDog future _) = receive future

instance Destructible (WatchDog a) where
   destroy (WatchDog _ mv) = setVar mv (False,always ())
   destroyed (WatchDog _ mv) = 
#if 0
      -- this appears to be wrong.  If you sync(destroyed(watchdog))
      -- before the watchdog is destroyed, you will simply be 
      -- waiting on inaction, so you will deadlock.
       
      event (
         do
            (_,event) <- getVar mv
            return event
         ) |>> done
#else
      error "WatchDog.destroyed used"
#endif

-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newWatchdog :: Duration -> IO (Maybe a) -> IO (WatchDog a)
newWatchdog time cmd =
-- repeatedly try command, with the indicated duration, until it
-- returns Just (something), and then make that available to the
-- watchdog channel.
   do
       mv <- newMVar (True,inaction)
       ft <- newFuture (sensor cmd mv)
       return (WatchDog ft mv)
   where 
      sensor cmd mv = 
         do
            st <- cmd
            (go,_) <- getVar mv
            unless go deadlock
            case st of
               Nothing -> delay time >> sensor cmd mv
               (Just a) -> return a

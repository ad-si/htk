 {- #########################################################################

MODULE        : InterActor
AUTHOR        : Einar W. Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : Interactors are iterative servers set up to handle events.
                The concept is a refinement of the Actor model of Gul Agha.

InterActors are basically packaged EventStream.  This file implements
code which actually creates them and then runs them.  There is no way of
getting rid of them once installed; they simply deadlock.

   ######################################################################### -}

module InterActor (
   module Interaction,
   
   Actor(..),
   HasBinding(..),
   
   InterActor,
   newInterActor,
   interactor,
   self,
   stop
   
   ) where

import Concurrency
import EventStream
import Interaction
import Dynamics
import Debug(debug)

-- --------------------------------------------------------------------------
--  Interactor Handle
-- --------------------------------------------------------------------------

data InterActor = InterActor {self::ThreadID, eventstream:: (EventStream ())}
-- self comes from a special thread which is forked for the purpose.
-- This provides a unique identifier.  But
-- the Ord and Eq implementations don't use it!

-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

-- Eq and Ord eventually turn out to turn on the objectid assigned
-- to the underlying Listener.
instance Eq InterActor where
   iact1 == iact2 = (eventstream iact1) == (eventstream iact2)

instance Ord InterActor where
   iact1 <= iact2 = (eventstream iact1) <= (eventstream iact2)

instance Show InterActor where
   showsPrec d iact r = show (eventstream iact) ++ r

instance EventListener InterActor where
   toListener = toListener . eventstream
   reply iact = reply (eventstream iact)

instance Actor InterActor (IA ()) where
   become iact e = become (eventstream iact) e

instance HasBinding InterActor () where
-- as noted before, this interface doesn't actually seem to be used
-- anywhere.
   bind iact = bind (eventstream iact)
   unbind iact = unbind (eventstream iact) 
                
-- --------------------------------------------------------------------------
--  Interactor Creation and Behaviour
-- --------------------------------------------------------------------------

newInterActor :: (InterActor -> IA ()) -> IO InterActor 
-- set up new interactor, returning it, and also set it going.
newInterActor f = 
   do
      mv <- newEmptyMVar
      forkIO ( 
         do
            es <- newEventStream
            tid <- getThreadID
            let iact = InterActor {self = tid,eventstream = es}
            become iact (f iact) 
            putMVar mv iact
            -- this thread is now ready to handle the requests.  Do so
            -- repeatedly.
            dispatch iact
         )
      takeMVar mv  

interactor :: (InterActor -> IA ()) -> IO () 
-- Like newInteractor, but don't bother to return it.
interactor f =  
   do
      forkIO( 
         do
            es <- newEventStream
            tid <- getThreadID
            iact <- return (InterActor {self = tid,eventstream = es});
            become iact (f iact) 
            dispatch iact
         )
      return ()

-- --------------------------------------------------------------------------
--  Stop Interactor/Thread
-- --------------------------------------------------------------------------

stop :: InterActor -> IO a
stop iact = do 
   -- this also deadlocks.
   become iact (inaction :: IA ())
   -- that should deregister everything.
   reply iact
   deadlock


-- --------------------------------------------------------------------------
--  Event Dispatching/Iterative Choice
-- --------------------------------------------------------------------------

dispatch :: InterActor -> IO ()
dispatch iact @ (InterActor {eventstream=es}) = 
   do
      -- if there is an error in the action stop immediately.
      catch (receiveIO es) (const (stop iact))
      dispatch iact

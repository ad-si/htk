 {- #########################################################################

MODULE        : InterActor
AUTHOR        : Einar W. Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : Interactors are iterative servers set up to handle events.
                The concept is a refinement of the Actor model of Gul Agha.


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

-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

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
        bind iact = bind (eventstream iact)
        unbind iact = unbind (eventstream iact) 
                
-- --------------------------------------------------------------------------
--  Interactor Creation and Behaviour
-- --------------------------------------------------------------------------

newInterActor :: (InterActor -> IA ()) -> IO InterActor 
newInterActor f = do {
        mv <- newEmptyMVar;
        forkIO ( do {
                es <- newEventStream;
                tid <- getThreadID;
                iact <- return (InterActor tid es);
                become iact (f iact); 
                putMVar mv iact; 
                dispatch iact;
                });
        takeMVar mv
        }  

interactor :: (InterActor -> IA ()) -> IO () 
interactor f =  forkIOnull ( do {
        es <- newEventStream;
        tid <- getThreadID;
        iact <- return (InterActor tid es);
        become iact (f iact); 
        dispatch iact;
        })

-- --------------------------------------------------------------------------
--  Stop Interactor/Thread
-- --------------------------------------------------------------------------

stop :: InterActor -> IO a
stop iact = do 
        become iact (inaction :: IA ())
        reply iact
        deadlock


-- --------------------------------------------------------------------------
--  Event Dispatching/Iterative Choice
-- --------------------------------------------------------------------------

dispatch :: InterActor -> IO ()
dispatch iact @ (InterActor _ es) = do
        catch (receiveIO es) (const (stop iact))
        dispatch iact

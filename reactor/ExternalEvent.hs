{- #########################################################################

MODULE        : ExternalEvent
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Interactions with the environment.

An EE event is something which can do something with Listeners.
(So you supply it with a Listener and it can register/deregister
the listener).  An IA is basically a combination of internal
events (as implemented by Selective.hs) and external events.
In Interaction.hs it will be made possible to sync on these.


   ######################################################################### -}


module ExternalEvent(
   IA(..), -- Interactor type
   EE(..), -- type of external events (which include a continuation function)
   Action(..), -- type of continuation function in events
   Register, -- type of register function
   Deregister, -- ditto deregister
   deregisterEvents, -- deregister a list of events with a listener.
   registerEvents -- ditto register
   )
 where

import Concurrency
import Dynamics
import Listener
import Debug(debug)

-- --------------------------------------------------------------------------
-- Syntactic Domains: Events
-- --------------------------------------------------------------------------

data IA a = IA (IO () -> EV a) [EE a]
-- An IA a is a list of events of two types.  
-- The first
-- component is a list of internal events.  The IO action passed
-- to it is intended to deregister interest in the external
-- events should an internal event occur.
-- The second is a list of external events.
data EE a = EE EventID (Action a) 

data Action a = Action Register Deregister (Dyn -> IO a)

-- Register and Deregister thingies seem to be implemented by
-- for example the EventBroker module.
type Register = Listener -> IO ()

type Deregister = Register

-- --------------------------------------------------------------------------
-- External Events and Interactions
-- --------------------------------------------------------------------------

deregisterEvents :: [EE a] -> Listener -> IO ()
deregisterEvents externalEvents listener =
   foreach 
      externalEvents
      (\ (EE _ (Action _ deregister _)) -> try (deregister listener))

registerEvents :: [EE a] -> Listener -> IO ()
registerEvents externalEvents listener =
   foreach
      externalEvents
      (\ (EE _ (Action register _ _)) -> try (register listener)) 


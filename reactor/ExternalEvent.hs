{- #########################################################################

MODULE        : ExternalEvent
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Interactions with the environment.


   ######################################################################### -}


module ExternalEvent where

import Concurrency
import Dynamics
import Listener
import Debug(debug)

-- --------------------------------------------------------------------------
-- Syntactic Domains: Events
-- --------------------------------------------------------------------------

data IA a = IA (IO () -> EV a) [EE a] 

type EE a = (EventID, Action a) 

type Action a = (Register, Deregister, Dyn -> IO a)

type Register = Listener -> IO ()

type Deregister = Register

-- --------------------------------------------------------------------------
-- External Events and Interactions
-- --------------------------------------------------------------------------

deregisterEvents ::[EE a] -> Listener -> IO ()
deregisterEvents el lst = foreach el (\(_,(_,dereg,_)) -> try(dereg lst))

registerEvents ::[EE a] -> Listener -> IO ()
registerEvents el lst = foreach el (\(_,(reg,_,_)) -> try(reg lst))


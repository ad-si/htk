
{- #########################################################################

MODULE        : Event
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Event Class and related operators

   ######################################################################### -}



module Event (
        Functor(..),
        Event(..),

        syncSequence_,
        syncSequence,

        choose,
        select,
        eventually,

        whenEV,
        unlessEV,

        catchEV

        ) where

import Debug(debug)

import Computation(Answer)

-- --------------------------------------------------------------------------
-- Event Class System
-- --------------------------------------------------------------------------

infixr 1 >>>=
infixr 1 >>>
infixr 0 +>

class Functor e => Event e where
        inaction :: e a
        sync     :: e a -> IO a
        poll     :: e a -> IO (Maybe a)
        (>>>=)   :: e a -> (a -> IO b) -> e b
        (>>>)    :: e a -> IO b -> e b
        (+>)     :: e a -> e a -> e a
        tryEV    :: e a -> e (Answer a)
        e >>> c = e >>>= (\ _ -> c)
        fromIO   :: IO a -> e a
        -- fromIO converts an action into an event
        -- which is always available and returns the result of
        -- the supplied action.

instance Event e => Monad e where
   -- This is to allow us to use the "do" notation.  This means we
   -- need sensible definitions for just ">>=".  ("fail" is already
   -- defined to be "error" which is OK; ">>" is defined in terms of
   -- ">>=")
        (>>=) ev1 ev2fun =
           ev1 >>>=
              ( \ ev1res -> sync(ev2fun ev1res)
                 )
        return value = fromIO (return value)

-- --------------------------------------------------------------------------
--  Synchronization
-- --------------------------------------------------------------------------

syncSequence_ :: Event e => [e a] -> IO ()
syncSequence_ = sequence_ . (map sync)

syncSequence :: Event e => [e a] -> IO [a]
syncSequence = sequence . (map sync)


-- --------------------------------------------------------------------------
-- Choice
-- --------------------------------------------------------------------------

choose :: Event e => [e a] -> e a
choose = foldr (+>) inaction


select :: Event e => [e a] -> IO a
select = sync . choose


eventually :: Event e => e a -> e a -> e a
eventually e1 e2 =
        e1 >>> sync (eventually e1 e2)
    +>  e2

-- --------------------------------------------------------------------------
-- Guarded Event (Pre-condition)
-- --------------------------------------------------------------------------

whenEV :: Event e => Bool -> e a -> e a 
whenEV p e = if p then e else inaction

unlessEV :: Event e => Bool -> e a -> e a
unlessEV p e = whenEV (not p) e


-- --------------------------------------------------------------------------
-- Error Handling
-- --------------------------------------------------------------------------

catchEV     :: Event e => e a -> (IOError -> IO a) -> e a
catchEV e h = tryEV e >>>= either h return

{- #########################################################################

MODULE        : Interaction
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Interactions with the environment represented as first class
                composable event values.

Implements glue on IA type defined in ExternalEvent.hs

Interaction.hs implements IA's as things you sync/poll on.
EventStream.hs/InterActor.hs implements extra glue around IA's (introducing
   a finite map) and makes them things you interact on.  There is no
   real reason I can see why we don't just write an interactor using
   the code in this file, except efficiency.

   ######################################################################### -}


module Interaction (
   EventID(..),
   ObjectID,
   EventPatternID,
   EventDesignator(..),

   EventListener(..),
   Listener,
   Register,
   Deregister,

   IA,
   interaction,
   (|>>=),
   (|>>),
   (\>),
   lift,
   ) where

import List

import ExtendedPrelude(monadDot)
import Concurrency
import Listener
import ExternalEvent
import Object
import Dynamics
import Event
import Debug(debug)

-- --------------------------------------------------------------------------
-- Operators
-- --------------------------------------------------------------------------

infixr 1 |>>=
infixr 1 |>>
infixl 0 \>

-- --------------------------------------------------------------------------
-- Functor Instance
-- --------------------------------------------------------------------------

instance Functor IA where
   fmap f e  = e >>>= return . f

-- --------------------------------------------------------------------------
-- Instance: Events
-- --------------------------------------------------------------------------

instance Event.Event ExternalEvent.IA where
   inaction = IA (const Event.inaction) []
        
   (>>>=) (IA internals externals) continuation = 
      IA 
         (\ioact -> (internals ioact) >>>= continuation) 
         (map (mapEE continuation) externals)
      where 
         mapEE 
            continuation 
            (EE e (Action register deregister oldContinuation)) =
            (EE e (Action register deregister 
               (continuation `monadDot` oldContinuation)))
   (IA internals1 externals1) +> (IA internals2 externals2) = 
      IA (\f -> internals1 f +> internals2 f) (externals1 ++ externals2)
   tryEV (IA internals externals) = 
      IA (\ioact -> tryEV (internals ioact)) (map tryEE externals)
      where 
         tryEE 
            (EE eventId (Action register deregister resp)) = 
            (EE eventId (Action register deregister (try . resp)))

   sync = pollOrSync Event.sync
   poll = pollOrSync Event.poll
   
   fromIO (action::IO a) = 
      let
         triv = (fromIO action) :: EV a
      in
         lift triv

-- --------------------------------------------------------------------------
--  External Event
-- --------------------------------------------------------------------------

-- interaction creates a new interaction given register+deregister functions
-- plus an event id.
interaction :: (EventDesignator e, Typeable a) => e -> Register -> Deregister -> IA a
interaction eventDesignator register deregister = 
   IA (const inaction) [EE eventId (Action register deregister coerceIO)]
   where 
      eventId    = toEventID eventDesignator

-- --------------------------------------------------------------------------
--  Lifting Channel Events
-- --------------------------------------------------------------------------

(|>>=) :: EV a -> (a -> IO b) -> IA b
e |>>= f = lift e >>>= f

(|>>) :: EV a -> IO b -> IA b
e |>> c = e |>>= (const c)

(\>) :: IA a -> IA a -> IA a 
-- Remove all external events in second argument from first argument
(IA internals externals) \> (IA _ externals') = 
      IA internals (filter (notIn externals') externals)
   where 
      notIn externals' (EE eventId _) = 
         not (any (\ (EE eventId' _ ) -> eventId == eventId') externals' )


lift :: EV a -> IA a
lift internals = 
   IA (\iact -> 
      tryEV internals >>>= 
         \ ans -> 
            do 
               iact
               propagate ans
      ) []

-- --------------------------------------------------------------------------
-- External Events and Interactions
-- --------------------------------------------------------------------------

pollOrSync :: (EV eventOut -> IO result) -> (IA eventOut) -> IO result
pollOrSync pOrS (IA internals []) = pOrS (internals done)
pollOrSync pOrS (IA internals externals) = 
   do
      listener <- newListener
      registerEvents externals listener
      let deregistration = deregisterEvents externals listener
      pOrS(
         receive (msgchannel listener) >>>= 
            (\ message ->
               do 
                  deregistration
                  handleEvent listener externals message 
               )
         +>   
         (internals deregistration)
         )
  


handleEvent :: Listener -> [EE a] -> Message -> IO a
handleEvent _ externals (Message Notice eventId info) = 
   execAction externals eventId info
handleEvent listener externals (Message Oneway eventId info) = 
   do 
      sendIO (replychannel listener) ()
      execAction externals eventId info
handleEvent listener externals (Message Request eventId info) = 
   do
      ans <- try (execAction externals eventId info)
      sendIO (replychannel listener) ()
      propagate ans

execAction :: [EE a] -> EventID -> Dyn -> IO a
execAction externals eventId info = 
   case find (\ (EE eventId' _) -> (eventId' == eventId)) externals of
      Nothing -> error "Missing Binding in call to sync"
      Just (EE _ (Action _ _ response)) -> response info
{- Actually, one should make a non-deterministic choice here between one
of the possible event-action bindings for the event in question 
-}                      









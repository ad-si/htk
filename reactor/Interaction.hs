{- #########################################################################

MODULE        : Interaction
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : 1.0
DESCRIPTION   : Interactions with the environment represented as first class
                composable event values.


   ######################################################################### -}


module Interaction (
        EventID(..),
        ObjectID,
        EventPatternID(..),
        EventDesignator(..),

        EventListener(..),
        Listener,
        Register(..),
        Deregister(..),

        IA,
        interaction,
        (|>>=),
        (|>>),
        (\>),
        lift,
        ) where


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
        (>>>=) (IA e ex) f = 
                IA (\c -> (e c) >>>= f) (map (mapEE f) ex)
                where mapEE f (e, (reg,dereg,resp)) = 
                        (e, (reg,dereg,\v -> resp v >>= f))     
        (IA e1 ex1) +> (IA e2 ex2) = IA (\f -> e1 f +> e2 f) (ex1 ++ ex2)
        tryEV (IA e ex) = 
                IA (\c -> tryEV (e c)) (map tryEE ex)
                where tryEE (e, (reg,dereg,resp)) = 
                        (e, (reg,dereg,\v -> try(resp v)))
        sync (IA e []) = sync (e done)
        sync (IA e ex) = do
                lst <- newListener
                registerEvents ex lst
                sync (
                        receive (msgchannel lst) >>>= (\v -> do {
                                deregisterEvents ex lst;
                                handleEvent lst ex v
                                })
                   +>   (e (deregisterEvents ex lst))
                   )
        poll (IA e []) = poll (e done)
        poll (IA e ex) = do
                lst <- newListener
                registerEvents ex lst
                poll (
                        receive (msgchannel lst) >>>= (\v -> do {
                                deregisterEvents ex lst;
                                handleEvent lst ex v
                                })
                   +>   (e (deregisterEvents ex lst))
                   )



-- --------------------------------------------------------------------------
--  External Event
-- --------------------------------------------------------------------------

interaction :: (EventDesignator e, Typeable a) => e -> Register -> Deregister -> IA a
interaction e reg dereg = 
   IA (const inaction) [(eid,(reg,dereg,resp))]
   where eid    = toEventID e
         resp d = coerceIO d


-- --------------------------------------------------------------------------
--  Lifting Channel Events
-- --------------------------------------------------------------------------

(|>>=) :: EV a -> (a -> IO b) -> IA b
e |>>= f = lift e >>>= f

(|>>) :: EV a -> IO b -> IA b
e |>> c = e |>>= (\ _ -> c)

(\>) :: IA a -> IA a -> IA a                    -- restriction
(IA e ial) \> (IA _ ial') = IA e (filter (notIn ial') ial)
  where notIn ial' (eid,_) = not (any (\(eid',_) -> eid == eid') ial')


lift :: EV a -> IA a
lift e = IA (\c -> tryEV e >>>= \ans -> do {c;propagate ans}) []

-- --------------------------------------------------------------------------
-- External Events and Interactions
-- --------------------------------------------------------------------------

handleEvent :: Listener -> [EE a] -> Message -> IO a
handleEvent _ ee (Message Notice eid info) = 
        execAction ee eid info
handleEvent lst ee (Message Oneway eid info) = do 
        sendIO (replychannel lst) ()
        execAction ee eid info
handleEvent lst ee (Message Request eid info) = do
        ans <- try (execAction ee eid info)
        sendIO (replychannel lst) ()
        propagate ans

execAction :: [EE a] -> EventID -> Dyn -> IO a
execAction eel eid info = 
        case filter (\(eid',_) -> eid' == eid) eel of
                [] -> error "Missing Binding in call to sync"
                (_,(_,_,resp)) : _ -> (resp info)


{- Actually, one should make a non-deterministic choice here between one
of the possible event-action bindings for the event in question 
-}                      

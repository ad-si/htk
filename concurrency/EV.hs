{- This file implements the EV event type as used in UniForM.
   EV is based on BaseEvent's which in turn are based on
   PrimEvent's.  Certain events, such as withTimeOut,
   are not defined in this file, because we need channels,
   which will be defined in Channels.hs.
   -}
module EV (
   module EV,
   
   EV, 
   -- the EV type.  The most important thing is that this implements
   -- the Event interface.

   deadlock, 
   -- wait forever (achieved by syncing on the empty event)
   event, -- :: IO (EV a) -> EV a ; this puts off computing an event
   -- until we sync on it, as needed for example by whenGuard and
   -- unlessGuard

   whenGuard,  -- :: IO Bool -> EV a -> EV a
   unlessGuard, -- :: IO Bool -> EV a -> EV a
   -- whenGuard,unlessGuard qualify an event by a boolean computation
   -- run when it is sync'ed.

   baseEventToEV -- :: BaseEvent -> EV
   -- make an event from a base event.

   ) where

import IO(try)
import Monad(liftM)

import qualified Debug(debug,(@:))
import Event
import BaseEvent
import IOtoBaseEvent
import Dynamics

data EV eventResult = EV [BaseEvent eventResult] [IO (EV eventResult)]
-- Structure containing base events.  The IO'things make this
-- dependent on interaction with the rest of the world (done
-- when the sync or poll starts).  If the IO raises an error, this is
-- simply ignored and treated as if it returned [].

-- The process of extracting the base events is done by
-- resolveEV.
resolveEV :: EV eventResult -> IO [BaseEvent eventResult]
resolveEV (EV static dynamic) =
   do
      let safeDynamic = map makeSafe dynamic
      getDynamic <- sequence safeDynamic
      resolvedDynamic <- mapM resolveEV getDynamic

      return(concat(static:resolvedDynamic))
   where
      makeSafe dynamicItem = 
         do
            result <- try dynamicItem
            case result of
               Left error ->
                  do
                     Debug.debug "EV.makeSafe"
                     return inaction
               Right list -> return list

-- mapEV is also useful . . .
mapEV :: (BaseEvent a -> BaseEvent b) -> (EV a -> EV b)
mapEV baseMap (EV static dynamic) =
   EV (map baseMap static) (map (liftM (mapEV baseMap)) dynamic)

instance Functor EV where -- this needs to be done for the definition of 
-- Event.  I don't know why Event's have to be functors.
   fmap f e  = e >>>= return . f

instance Event EV where
   inaction = EV [] []

   (>>>=) event converter = mapEV (>>>=# converter) event
  
   tryEV event = mapEV tryBaseEvent event

   (+>) (EV static dynamic) (EV static' dynamic') = 
      EV (static ++ static') (dynamic ++ dynamic')

   poll event =
      do
         baseEvents <- resolveEV event
         pollBaseEvents baseEvents

   sync event =
      do
         baseEvents <- resolveEV event
         syncBaseEvents baseEvents        

   fromIO action = EV [ioToBaseEvent action] []   

deadlock :: IO eventResult
deadlock =
   do
      Debug.debug "EV.deadlock called"
      sync (inaction :: EV eventResult)

baseEventToEV :: BaseEvent eventResult -> EV eventResult
baseEventToEV baseEvent = EV [baseEvent] []

event :: IO (EV a) -> EV a
event action = EV [] [action]

whenGuard :: IO Bool -> EV a -> EV a
whenGuard guard toGuard = 
   event(
      do
         condition <- guard
         return(whenEV condition toGuard)
      )

unlessGuard guard toGuard =
   event(
      do
         condition <- guard
         return(unlessEV condition toGuard)
      )

-- So we can send EV values over IA channels.
instance Typeable val => Typeable (EV val) where
   typeOf _ =
      let
         sampleValue  = error "EV.pleasedon'tevaluate" :: val
      in
         mkTypeTag tagfor_EV [typeOf sampleValue]
   
tagfor_EV = mkTyCon "EV" "EV"
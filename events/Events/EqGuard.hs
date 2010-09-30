{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Here we create a simple guarded queue which allows guarding by equality
-- according to an ordered key.  Thus guards have three values,
-- match anything, match nothing, and match this value.
--
-- To simplify the implementation, we specify that an Eq match has higher
-- priority than a MatchAnything match, and when we must choose between
-- values for MatchAnything, do not necessarily choose the first
-- (more likely the one with the lowest key value).  But we do respect
-- FIFO order when only Eq guards are involved.
module Events.EqGuard(
   EqGuardedChannel, -- the channel
   EqMatch(..), -- the guard.
   newEqGuardedChannel, -- construct a channel
   ) where

import Util.Computation

import Events.GuardedEvents
import Events.GuardedChannels
import Events.DeleteQueue
import Events.FMQueue

type EqGuardedChannel key value = GuardedChannel (EqMatch key) (key,value)

newEqGuardedChannel :: Ord key => IO (EqGuardedChannel key value)
newEqGuardedChannel =
   newEqGuardedChannelPrim (error "EqGuard.1") (error "EqGuard.2")

newEqGuardedChannelPrim :: Ord key => key -> value
   -> IO (EqGuardedChannel key value)
-- The arguments to newEqGuardedChannelPrim are not looked at, but
-- help us to avoid overloading woes.
newEqGuardedChannelPrim (_::key) (_ ::value) =
   newGuardedChannel (error "newEq1" :: (GQ (EqGuardQueue key) (key,value)))
      (error "newEq2" :: (VQ (EqValueQueue key value)))

-- --------------------------------------------------------------------
-- The Guard type
-- --------------------------------------------------------------------

data EqMatch key =
      Eq !key
   |  EqMatchAny
   |  EqMatchNone

instance Ord key => Guard (EqMatch key) where
   nullGuard = EqMatchAny

   andGuard EqMatchAny x = x
   andGuard EqMatchNone x = EqMatchNone
   andGuard x EqMatchAny = x
   andGuard x EqMatchNone = EqMatchNone
   andGuard (Eq key1) (Eq key2) =
      if key1 == key2 then Eq key1 else EqMatchNone

-- --------------------------------------------------------------------
-- The value queue.
-- --------------------------------------------------------------------

newtype Ord key => EqValueQueue key value valueCont =
   EqValueQueue (FMQueue key ((key,value),valueCont))

instance Ord key => HasEmpty (EqValueQueue key value) where
   newEmpty = return (EqValueQueue emptyFMQueue)

instance Ord key => HasAdd (EqValueQueue key value) (key,value) where
   add (EqValueQueue fmQueue) keyValue@(key,value) valueCont =
      do
         (fmQueue2,invalidate) <- addFMQueue fmQueue key (keyValue,valueCont)
         return (EqValueQueue fmQueue2,invalidate)

instance Ord key => HasRemove (EqValueQueue key value) (EqMatch key)
      (key,value) where
   remove (EqValueQueue fmQueue) EqMatchAny =
      do
         (removed,fmQueue0) <- removeFMQueueAny fmQueue
         case removed of
            Nothing -> return (Nothing,EqValueQueue fmQueue0)
            (Just (_,(keyValue,valueCont),fmQueue2)) ->
               return (Just(keyValue,valueCont,
                     return (EqValueQueue fmQueue0)),
                  EqValueQueue fmQueue2)
   remove (EqValueQueue fmQueue) (Eq key) =
      do
         (removed,fmQueue0) <- removeFMQueue fmQueue key
         case removed of
            Nothing -> return (Nothing,EqValueQueue fmQueue0)
            (Just ((keyValue,valueCont),fmQueue2)) ->
               return (Just(keyValue,valueCont,
                     return (EqValueQueue fmQueue0)),
                  EqValueQueue fmQueue2)

-- --------------------------------------------------------------------
-- The Guard Queue
-- --------------------------------------------------------------------

data Ord key => EqGuardQueue key guardCont =
   EqGuardQueue {
      matchAnys :: DeleteQueue guardCont,
      eqs :: FMQueue key guardCont
      }

instance Ord key => HasEmpty (EqGuardQueue key) where
   newEmpty = return (EqGuardQueue {
      matchAnys = emptyQueue,
      eqs = emptyFMQueue
      })

instance Ord key => HasAdd (EqGuardQueue key) (EqMatch key) where
   add guardQueue guard guardCont =
      case guard of
         Eq key ->
            do
               let fmQueue = eqs guardQueue
               (fmQueue2,invalidate) <- addFMQueue fmQueue key guardCont
               return (guardQueue {eqs = fmQueue2},invalidate)
         EqMatchAny ->
            do
               let deleteQueue = matchAnys guardQueue
               (deleteQueue2,invalidate) <- addQueue deleteQueue guardCont
               deleteQueue3 <- cleanQueue deleteQueue2
               return (guardQueue {matchAnys = deleteQueue2},invalidate)
         EqMatchNone -> return (guardQueue,done)

instance Ord key => HasRemove (EqGuardQueue key) (key,value) (EqMatch key) where
   remove guardQueue (key,_) =
      do
         removed <- removeFMQueue (eqs guardQueue) key
         case removed of
            (Just (guardCont,fmQueue2),fmQueue0) ->
               do
                  let gq fmq = guardQueue {eqs = fmq}
                  return (Just(Eq key,guardCont,return(gq fmQueue0)),
                     gq fmQueue2)
            (Nothing,fmQueue0) ->
               do
                  let
                     mAs = matchAnys guardQueue
                     gq dq = EqGuardQueue {matchAnys = dq,eqs = fmQueue0}
                  removed2 <- removeQueue mAs
                  case removed2 of
                     Just (guardCont,dqueue2,dqueue0) ->
                        return (Just (EqMatchAny,guardCont,
                              return (gq dqueue0)),
                           gq dqueue2)
                     Nothing ->
                        return (Nothing,gq mAs)

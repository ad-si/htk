{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Here we implement a null guard channel that provides no guards,
-- but is hopefully useful as an example.
module Events.NullGuard (
   NullGuardedChannel,
   newNullGuardedChannel
   ) where

import Events.GuardedEvents
import Events.GuardedChannels
import Events.DeleteQueue



type NullGuardedChannel value = GuardedChannel () value

newNullGuardedChannel :: IO (NullGuardedChannel value)
newNullGuardedChannel = newNullGuardedChannelPrim (error "newNull")

-- The argument to newNullGuardedChannelPrim is not looked at,
-- but helps us to avoid overloading woes.
newNullGuardedChannelPrim :: value -> IO (NullGuardedChannel value)
newNullGuardedChannelPrim (_ :: value) =
   newGuardedChannel (error "newNull1" :: (GQ NullGuardQueue value))
      (error "newNull2" :: (VQ (NullValueQueue value)))


-- --------------------------------------------------------------------
-- The Guard type
-- --------------------------------------------------------------------

instance Guard () where
   nullGuard = ()
   andGuard _ _ = ()

-- --------------------------------------------------------------------
-- The Value Queue.
-- --------------------------------------------------------------------

data NullValueQueue value valueCont =
   NullValueQueue (DeleteQueue (value,valueCont))

emptyNullValueQueue :: NullValueQueue value a
emptyNullValueQueue = NullValueQueue emptyQueue

instance HasEmpty (NullValueQueue value) where
   newEmpty = return emptyNullValueQueue

instance HasAdd (NullValueQueue value) value where
   add (NullValueQueue deleteQueue) value valueCont =
      do
         (deleteQueue2,invalidate) <- addQueue deleteQueue (value,valueCont)
         return (NullValueQueue deleteQueue2,invalidate)

instance HasRemove (NullValueQueue value) () value where
   remove (NullValueQueue deleteQueue) () =
       do
          removed <- removeQueue deleteQueue
          case removed of
             Nothing -> return (Nothing,emptyNullValueQueue)
             Just ((value,valueCont),deleteQueue2,deleteQueue0) ->
                return (Just(value,valueCont,
                      return (NullValueQueue deleteQueue0)),
                   NullValueQueue deleteQueue2)
-- --------------------------------------------------------------------
-- The Guard Queue
-- --------------------------------------------------------------------

data NullGuardQueue guardCont = NullGuardQueue (DeleteQueue guardCont)

emptyNullGuardQueue :: NullGuardQueue a
emptyNullGuardQueue = NullGuardQueue emptyQueue

instance HasEmpty NullGuardQueue where
   newEmpty = return emptyNullGuardQueue

instance HasAdd NullGuardQueue () where
   add (NullGuardQueue deleteQueue) () guardCont =
      do
         (deleteQueue2,invalidate) <- addQueue deleteQueue guardCont
         deleteQueue3 <- cleanQueue deleteQueue2
         return (NullGuardQueue deleteQueue3,invalidate)

instance HasRemove NullGuardQueue value () where
   remove (NullGuardQueue deleteQueue) value =
       do
          removed <- removeQueue deleteQueue
          case removed of
             Nothing -> return (Nothing,emptyNullGuardQueue)
             Just (guardCont,deleteQueue2,deleteQueue0) ->
                return (Just((),guardCont,
                      return (NullGuardQueue deleteQueue0)),
                   NullGuardQueue deleteQueue2)




{- We implement Channels and Message Queues in this file.
   -}
module Channels(
   Channel,
   newChannel, -- :: IO (Channel a)

   MsgQueue, 
   newMsgQueue, -- :: IO (MsgQueue a)


   -- The following classes have both Channel and MsgQueue as instances
   HasReceiveEV(..),
   HasSendEV(..),
   HasReceiveIO(..),
   HasSendIO(..)
   ) where

import Event
import BaseEvent
import PrimEvent
import TwoWayChannel
import EV

class HasReceiveEV channel messageType where
   receive :: channel messageType -> EV messageType

class HasReceiveIO channel messageType where
   receiveIO :: channel messageType -> IO messageType

instance HasReceiveEV channel messageType => HasReceiveIO channel messageType 
      where
   receiveIO = sync . receive

class  HasSendEV channel messageType where
   send :: channel messageType -> messageType -> EV ()

class HasSendIO channel messageType where
   sendIO :: channel messageType -> messageType -> IO ()   

instance HasSendEV channel messageType => HasSendIO channel messageType 
      where
   sendIO channel message = sync(send channel message)

--------------------------------------------------------------------
-- Channels
--------------------------------------------------------------------

newtype Channel messageType = Channel(TwoWayChannel messageType ())

newChannel :: IO (Channel messageType)
newChannel = 
   do
      queue <- newTwoWayChannel
      return(Channel queue)

instance HasSendEV Channel messageType where
   send (Channel queue) message = 
      baseEventToEV(primToBaseEvent(leftEvent queue message))

instance HasReceiveEV Channel messageType where
   receive (Channel queue) =
      baseEventToEV(primToBaseEvent(rightEvent queue ()))

--------------------------------------------------------------------
-- Message Queues
-- We don't do it Einar's way - instead we simply put the
-- primitive send events through returnAtOnce
--------------------------------------------------------------------

newtype MsgQueue messageType = MsgQueue(TwoWayChannel messageType ())

newMsgQueue :: IO (MsgQueue messageType)
newMsgQueue = 
   do
      queue <- newTwoWayChannel
      return(MsgQueue queue)

instance HasSendEV MsgQueue messageType where
   send (MsgQueue queue) message = 
      baseEventToEV(primToBaseEvent(returnAtOnce(leftEvent queue message)))

instance HasReceiveEV MsgQueue messageType where
   receive (MsgQueue queue) =
      baseEventToEV(primToBaseEvent(rightEvent queue ()))








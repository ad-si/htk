{- WrapIO.wrapIO turns an IO action into an event (which has no
   non-trivial guards available). -}
module WrapIO(
   wrapIO, -- :: IO a -> IO (Event a,IO ())
      -- The IO action should be used when we are no longer interested
      -- in the event.
   ) where

import Computation

import NullGuard
import GuardedChannels
import Events
import Spawn

wrapIO :: IO a -> IO (Event a,IO ())
wrapIO (aAct :: IO a) =
   do
      channel <- newNullGuardedChannel
      let
         dispatcher =
            do
               next <- aAct
               sync(send channel next)
               dispatcher
      killAction <- spawn dispatcher
      let
         emptyChannel =
            do
               next <- poll(receive channel)
               case next of
                  Nothing -> done
                  Just _ -> emptyChannel
      return (receive channel,killAction >> emptyChannel)
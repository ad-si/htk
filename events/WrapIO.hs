-- | WrapIO.wrapIO turns an IO action into an event (which has no
-- non-trivial guards available). 
module WrapIO(
   wrapIO, -- :: IO a -> IO (Event a)
      -- Executes the action and provides an event which will be satisfied
      -- when the action finishes.

   wrapIORepeat, -- :: IO a -> IO (Event a,IO ())
      -- Like wrapIO but executes the action over and over again.  The
      -- returned action kills the thread in which this is done.

   impatientIO, -- :: IO a -> Duration -> IO (Maybe a)
      -- impatientIO is used to do an action with a time limit.
      -- (The action runs to completion though.)
   ) where

import Control.Exception

import Thread

import Channels
import Events
import Spawn

wrapIO :: IO a -> IO (Event a)
wrapIO (aAct :: IO a) =
   do
      channel <- newChannel
      let
         dispatcher =
            do
               next <- aAct
               sync(send channel next)
      spawn dispatcher
      let
         emptyChannel =
            do
               next <- poll(receive channel)
               case next of
                  Nothing -> done
                  Just _ -> emptyChannel
      return (receive channel)


wrapIORepeat :: IO a -> IO (Event a,IO ())
wrapIORepeat (aAct :: IO a) =
   do
      channel <- newChannel
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


impatientIO :: IO a -> Duration -> IO (Maybe a)
impatientIO action duration =
   do
      channel <- newChannel
      let
         timeOutThread = after duration (sendIO channel ())
      resultAct <- wrapIO (try action)
      forkIO timeOutThread
      
      let 
         passOn (Left error) = throw error
         passOn (Right val) = return (Just val)
      sync(
            resultAct >>>= passOn
         +> receive channel >>> return Nothing
         )   

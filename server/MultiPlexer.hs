-- | This module provides a general mechanism for bunching up commands to
-- a server and sending them in one go. 
module MultiPlexer(
   MultiPlexer,
   newMultiPlexer, 
      -- :: ([command] -> IO [response]) -> IO (MultiPlexer command response)
      -- The supplied action executes the given commands, and returns a
      -- list of responses to be dispatched; the list of responses should
      -- have the same length as the list of commands.
   sendCommand, -- :: MultiPlexer command response -> command -> IO response
   ) where

import Monad

import Control.Concurrent

import Queue
import Computation(done)

data MultiPlexer command response = MultiPlexer {
   queryFn :: [command] -> IO [response],
   stateMVar :: MVar (State command response)
   }

data State command response = State {
   busy :: Bool,
   queue :: Queue (command,MVar response)
      -- Invariant: when busy is False, the queue is empty.
   }

newMultiPlexer :: ([command] -> IO [response]) 
   -> IO (MultiPlexer command response)
newMultiPlexer queryFn =
   do
      stateMVar <- newMVar (State {busy = False,queue = emptyQ})
      let
         multiPlexer = MultiPlexer {
            queryFn = queryFn,
            stateMVar = stateMVar
            }
      return multiPlexer

sendCommand :: MultiPlexer command response -> command -> IO response
sendCommand (multiPlexer :: MultiPlexer command response) command =
   do
      responseMVar <- newEmptyMVar

      (alreadyBusy :: Bool) <- modifyMVar (stateMVar multiPlexer)
         (\ state -> 
            let
               queue1 = insertQ (queue state) (command,responseMVar)
               alreadyBusy = busy state
            in
               return (State {queue = queue1,busy = True},alreadyBusy)
            )

      if alreadyBusy
         then
            done
         else
            do
               yield
               unBusy True multiPlexer

      takeMVar responseMVar               

unBusy :: Bool -> MultiPlexer command response -> IO ()
unBusy doFork (multiPlexer :: MultiPlexer command response) =
   do
      (doNow :: Maybe [(command,MVar response)]) <-
         modifyMVar (stateMVar multiPlexer) (\ state ->
            return (if isEmptyQ (queue state)
               then
                  (state {busy = False},Nothing)
               else
                  (state {queue = emptyQ},Just (queueToList (queue state)))
               )
            )
      case doNow of
         Nothing -> done
         Just list ->
            let
               action =
                  do
                     responses <- queryFn multiPlexer (map fst list)
                     zipWithM_
                        (\ (_,responseMVar) response 
                           -> putMVar responseMVar response)
                        list responses
                     unBusy False multiPlexer
            in
               if doFork 
                  then
                     do
                        forkIO action
                        done
                  else
                     action

-- | Delayers handle delaying of actions; the main purpose is to delay
-- graph redrawing actions during complex updates.
module Util.Delayer(
   -- Client side
   Delayer,
   newDelayer, -- :: IO Delayer
   HasDelayer(..),
      -- :: Class of things which have a delayer.
      -- Delayer itself is an instance.
   delay, -- :: HasDelayer object => object -> IO a -> IO a
      -- carry out the given action preventing the Delayer from doing anything.

   -- Producer side
   DelayedAction,
   newDelayedAction, -- :: IO () -> IO DelayedAction
   delayedAct, -- :: Delayer -> DelayedAction -> IO ()
      -- If no delay is taking place, perform the DelayedAction action
      -- immediately.  Otherwise remember to do it when are no longer inside
      -- a delay.
      -- If the same DelayedAction is queued multiple times when a Delayer
      -- is delay'd, we nevertheless only do it once.
   cancelDelayedAct, -- :: Delayer -> DelayedAction -> IO ()
      -- If this DelayedAction is queued, remove it from the queue.

   HasAddDelayer(..),
   -- Instances of HasAddDelayer are event sources to which you can attach
   --    a delayer, to indicate you are currently not interested in events.

   HasAddDelayerIO(..),
   -- Like HasAddDelayer, but allows an IO action.
   ) where

import Control.Concurrent.MVar
import Control.Exception

import qualified Data.Set as Set

import Util.Object
import Util.Computation(done)

-- ------------------------------------------------------------------------
-- Data types
-- ------------------------------------------------------------------------

data DelayedAction = DelayedAction {
   oId :: ObjectID,
   action :: IO ()
   }

data DelayerState = DelayerState {
   delayCount ::  ! Int, -- ^ 0 when not delay'd.
   delayedActions :: Set.Set DelayedAction
   }

data Delayer = Delayer (MVar DelayerState)

-- ------------------------------------------------------------------------
-- HasAddDelayer
-- ------------------------------------------------------------------------

-- | Instances of HasAddDelayer are event sources to which you can attach
--   a delayer, to indicate you are currently not interested in events.
class HasAddDelayer eventSource where
   addDelayer :: Delayer -> eventSource -> eventSource

-- | Like HasAddDelayer, but allows an IO action.
class HasAddDelayerIO eventSource where
   addDelayerIO :: Delayer -> eventSource -> IO eventSource

-- ------------------------------------------------------------------------
-- HasDelayer
-- ------------------------------------------------------------------------

class HasDelayer object where
   toDelayer :: object -> Delayer

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance Eq DelayedAction where
   (==) act1 act2 = (==) (oId act1) (oId act2)

instance Ord DelayedAction where
   compare act1 act2 = compare (oId act1) (oId act2)

instance HasDelayer Delayer where
   toDelayer delayer = delayer

-- ------------------------------------------------------------------------
-- Client Side
-- ------------------------------------------------------------------------

newDelayer :: IO Delayer
newDelayer =
   do
      mVar <- newMVar emptyDelayerState
      return (Delayer mVar)

-- | carry out the given action preventing the Delayer from doing anything.
delay :: HasDelayer object => object -> IO a -> IO a
delay object action =
   do
      let
         delayer = toDelayer object
      beginDelay delayer
      finally action (endDelay delayer)

beginDelay :: Delayer -> IO ()
beginDelay (Delayer mVar) =
   modifyMVar_ mVar (\ delayerState0 ->
      do
         let
            delayCount1 = delayCount delayerState0 + 1

            delayerState1 = delayerState0 {delayCount = delayCount1}

         seq delayerState1 (return delayerState1)
      )

endDelay :: Delayer -> IO ()
endDelay (Delayer mVar) =
   do
      -- to reduce the danger of deadlocks, we don't perform the actions while
      -- the MVar is empty.
      afterAct <- modifyMVar mVar (\ delayerState0 ->
         do
            let
               delayCount1 = delayCount delayerState0 - 1
            return (if delayCount1 > 0
               then
                  (delayerState0 {delayCount = delayCount1},done)
               else
                  let
                     afterAct = mapM_
                        (\ delayedAction -> action delayedAction)
                        (Set.toList (delayedActions delayerState0))
                  in
                     (emptyDelayerState,afterAct)
               )
         )
      afterAct


emptyDelayerState :: DelayerState
emptyDelayerState = DelayerState {
   delayCount = 0,
   delayedActions = Set.empty
   }


-- ------------------------------------------------------------------------
-- Producer side
-- ------------------------------------------------------------------------

newDelayedAction :: IO () -> IO DelayedAction
newDelayedAction action =
   do
      oId <- newObject
      let
         delayedAction = DelayedAction {
            oId = oId,
            action = action
            }

      return delayedAction

-- } If no delay is taking place, perform the DelayedAction action
-- immediately.  Otherwise remember to do it when are no longer inside
-- a delay.
-- If the same DelayedAction is queued multiple times when a Delayer
-- is delay'd, we nevertheless only do it once.
delayedAct :: Delayer -> DelayedAction -> IO ()
delayedAct (Delayer mVar) delayedAct =
   do
      afterAct <- modifyMVar mVar (\ delayerState0 ->
         return (
            if delayCount delayerState0 == 0
               then
                  (delayerState0,action delayedAct)
               else
                  let
                     delayedActions1 = Set.insert delayedAct
                        (delayedActions delayerState0)

                     delayerState1 = delayerState0 {
                        delayedActions = delayedActions1
                        }
                  in
                     (delayerState1,done)
            )
         )
      afterAct

-- | If this DelayedAction is queued, remove it from the queue.
cancelDelayedAct :: Delayer -> DelayedAction -> IO ()
cancelDelayedAct (Delayer mVar) delayedAction =
   modifyMVar_ mVar (\ delayerState0 ->
      let
         delayedActions1
            = Set.delete delayedAction (delayedActions delayerState0)

         delayerState1 = delayerState0 {delayedActions = delayedActions1}
      in
         return delayerState1
      )

{- PrimEvent is the most basic event type, from which other UniForM
   events are built.  In this module we define PrimEvent's and implement
   a sync operation for them.
   -}
module PrimEvent(
   syncPrimEvents,
   PrimEventSelection(..), 
   -- wraps a primitive event + pair.  BaseEvent constructs elements
   -- of this type.
   returnAtOnce, -- :: PrimEvent eventResult -> PrimEvent ()
   -- This alters a PrimEvent to one which returns immediately
   -- but still registers the original event to be done at some time
   -- in the future.  This is intended for message queues, which
   -- we construct from channels and which have a send event which
   -- returns immediately, rather than waiting for a corresponding receive
   -- event.

   -- The remaining names are part of the interface for modules
   -- which define new primitive events.
   PrimEvent(..),
   PrimEventArg(..),
   Result(..)
   ) where

import Maybe

import Computation (done)
import Toggle

data Result = Immediate  | Awaiting
data PrimEvent eventResult = 
   PrimEvent (PrimEventArg eventResult -> IO Result)

data PrimEventArg eventResult = PrimEventArg (Toggle,eventResult -> IO())
{- Consider a primitive event, PrimEvent registerFun.
   registerFun(PrimEventArg(pendingFlag,action)) is called by 
   syncPrimEvents.  Since syncPrimEvents is given a list of primitive events,
   and only wants one to be actually received, it creates a
   single pendingFlag, initially True, which is the first
   argument to registerFun.  A primitive event handler should
   first atomically change pendingFlag from True to False. 
   Assuming that succeeds, it is then committed to running action. 
   Since action may occur in any thread and will block that thread, 
   it must be as short as possible and should not on any account raise
   an exception.

   If the result returned by registerFun is Immediate, that
   means that the pendingFlag is (at the time of return) already False,
   meaning that either this particular event executed immediately, or
   some other one has already executed.
   -}

data PrimEventSelection = forall eventResult .
   PrimEventSelection (PrimEvent eventResult,eventResult -> IO ())

returnAtOnce :: PrimEvent eventResult -> PrimEvent ()
-- documented in module export list
returnAtOnce(PrimEvent syncFun) =
   let
      newSyncFun (PrimEventArg (toggle,continuation)) =
         do
            stillToDo <- toggle1 toggle
            if stillToDo
               then
                  do
                     toggle2 <- newToggle
                     syncFun(PrimEventArg(toggle2,\ _ -> return ()))
                     continuation ()
               else
                  done
            return Immediate  
   in
      PrimEvent newSyncFun

syncPrimEvents :: [PrimEventSelection] -> Maybe (IO ()) -> IO ()
{- syncPrimEvent [(primEvent,action)] -> pollAction 
   takes a sequence of events with an associated action.  If one
   of them occurs immediately it executes the associated action.
   Otherwise it looks at pollAction.  If pollAction is Just action,
   it disables the other events (by toggling the the pendingFlag)
   and executes action.  Otherwise it returns anyway.
   -}
syncPrimEvents eventsList pollAction =
   do
     pendingFlag <- newToggle
     let
        syncPrimEventsList :: [PrimEventSelection] -> IO Bool
        syncPrimEventsList 
           (PrimEventSelection(PrimEvent syncFun,action) : otherEvents) =
           {- syncPrimEventsList is the first stage - register the events,
              until possibly one is executed.  It returns True if no more
              needs to be done. -}
           do
              result <- syncFun(PrimEventArg(pendingFlag,action))
              case result of
                 Immediate -> return True
                 Awaiting -> syncPrimEventsList otherEvents
        syncPrimEventsList [] = return False

     firstTry <- syncPrimEventsList eventsList
     if firstTry
        then
           done -- action already done
        else 
           case pollAction of
              Nothing -> done -- action maybe not done, but no poll action
              Just pollAct ->
                 do
                    grabEvent <- toggle1 pendingFlag
                    if grabEvent
                       then -- still not done, now do it.
                          pollAct
                       else
                          done
                      
               
              
  

   




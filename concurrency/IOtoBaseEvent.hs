{- IOtoBaseEvent.ioToBaseEvent takes a computation to a 
   BaseEvent which always executes that computation and returns 
   the resulting value whenever sync is called.
   -}
module IOtoBaseEvent(
   ioToBaseEvent -- :: IO a -> BaseEvent a
   ) where

import Toggle
import PrimEvent
import BaseEvent
import Debug(debug)

ioToBaseEvent :: IO a -> BaseEvent a
ioToBaseEvent action  = primToBaseEvent (ioToPrimEvent action)

ioToPrimEvent :: IO a -> PrimEvent a
ioToPrimEvent action =
      PrimEvent syncFun
   where
      syncFun(PrimEventArg (toggle,continuation)) =
         do
            toDo <- toggle1 toggle
            if toDo
               then
                  do
                     value <- action
                     continuation value
               else
                  return ()
            return Immediate   

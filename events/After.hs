{- After.hs implements the event which waits a given time and
   then executes an action.  It only works for GHC.
   -}
module After(after) where

import Concurrent

import Computation

import Events

-- after waits (approximately) the given number of microseconds.
after :: Int -> IO a -> Event a
after delay aAction =
   if delay <=0 
      then
         always aAction
      else 
         Event (
            \ toggle aActSink ->
               do
                  waitThread <- forkIO (
                     do
                        threadDelay delay
                        let Event registerFn = always aAction
                        Immediate <- registerFn toggle aActSink
                        done 
                     )
                  return (Awaiting done)
            )

{- The Guard class implements things that are supposed to be guards.  This
   means that they are supposed to implement a way of indexing things -}
module Guard(
   ) where

import FiniteMap


data Result = Immediate | Awaiting

data PrimEvent eventResult = PrimEvent (PrimEventArg eventResult -> IO Result)

data PrimEventArg eventResult = PrimEventArg (Toggle,eventResult -> IO ())


class GuardedEventSource




{- A GuardMap is used to represent the events waiting on a particular
   data source. -}
datatype GuardMap guard value data = GuardMap {
   addGuard :: guard -> GuardMap guard value data,
   query :: GuardMap guard value data -> value -> 
      Maybe (data,GuardMap guard value data)
   }

instance Guard guard value where
   emptyGuardMap :: GuardMap guard value data


class Guard guard where
   (&&&) :: guard -> guard -> guard

datatype Ord value => GuardOrd value where
   

class  guard
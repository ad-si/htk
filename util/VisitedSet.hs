module VisitedSet(
   VisitedSet,
   newVisitedSet, -- :: Ord key => IO (VisitedSet key)
   isVisited,
      -- :: Ord key => VisitedSet key -> key -> IO Bool
      -- return True if the element has already been visited, otherwise
      -- visit it.
   ) where

import Control.Concurrent
import DeprecatedSet

newtype VisitedSet key = VisitedSet (MVar (Set key))

newVisitedSet :: Ord key => IO (VisitedSet key)
newVisitedSet =
   do
      mVar <- newMVar emptySet
      return (VisitedSet mVar)

isVisited :: Ord key => VisitedSet key -> key -> IO Bool
isVisited (VisitedSet mVar) key =
   modifyMVar mVar
      (\ set ->
         return (if elementOf key set
            then
               (set,True)
            else
               (addToSet set key,False)
            )
         )

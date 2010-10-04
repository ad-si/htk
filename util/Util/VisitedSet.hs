module Util.VisitedSet(
   VisitedSet,
   newVisitedSet, -- :: Ord key => IO (VisitedSet key)
   isVisited,
      -- :: Ord key => VisitedSet key -> key -> IO Bool
      -- return True if the element has already been visited, otherwise
      -- visit it.
   ) where

import Control.Concurrent
import qualified Data.Set as Set

newtype VisitedSet key = VisitedSet (MVar (Set.Set key))

newVisitedSet :: Ord key => IO (VisitedSet key)
newVisitedSet =
   do
      mVar <- newMVar Set.empty
      return (VisitedSet mVar)

isVisited :: Ord key => VisitedSet key -> key -> IO Bool
isVisited (VisitedSet mVar) key =
   modifyMVar mVar
      (\ set ->
         return (if Set.member key set
            then
               (set,True)
            else
               (Set.insert key set,False)
            )
         )

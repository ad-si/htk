-- | A DelSet a implements a keyed set from which objects can be deleted. 
module DelSet (
   DelSet,
   DelSetKey, -- type of index
   emptyDelSet, -- :: DelSet a
   addDelSet, -- :: DelSet a -> a -> (DelSet a,DelSetKey)
   listDelSet, -- :: DelSet a -> [a]
   delFromDelSet, -- :: DelSet a -> DelSetKey -> DelSet a
      -- deleting if the item is not there does nothing.
   lookupDelSet, --  :: DelSet a -> DelSetKey -> Maybe a
      -- returns an item by its key.

   toDelSetKey, -- :: Int -> DelSetKey
   fromDelSetKey, -- :: DelSetKey -> Int
   ) where

import FiniteMap

import Object -- we get our keys from here.

data DelSet a = DelSet !Int (FiniteMap Int a)

newtype DelSetKey = DelSetKey Int

toDelSetKey :: Int -> DelSetKey
toDelSetKey = DelSetKey

fromDelSetKey :: DelSetKey -> Int
fromDelSetKey (DelSetKey i) = i

emptyDelSet :: DelSet a
emptyDelSet = DelSet 0 emptyFM

addDelSet :: DelSet a -> a -> (DelSet a,DelSetKey)
addDelSet (DelSet next fmap) val =
   (DelSet (next+1) (addToFM fmap next val),DelSetKey next)

listDelSet :: DelSet a -> [a]
listDelSet (DelSet next fmap) = eltsFM fmap

lookupDelSet :: DelSet a -> DelSetKey -> Maybe a
lookupDelSet (DelSet next fmap) (DelSetKey key) = lookupFM fmap key

delFromDelSet :: DelSet a -> DelSetKey -> DelSet a
delFromDelSet (DelSet next fmap) (DelSetKey key) =
   DelSet next (delFromFM fmap key)

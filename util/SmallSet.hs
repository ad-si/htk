{- SmallSets are a set-like type on small numbers of elements.
   It's being written for SharedGraph where the Set type seems
   too heavyweight.
   -}
module SmallSet(
   SmallSet,
   emptySmallSet,
      -- :: Ord elt => SmallSet elt
   addSmallSet,
      -- :: Ord elt => elt -> SmallSet elt -> SmallSet elt
   addSmallSetList,
      -- :: Ord elt => [elt] -> SmallSet elt -> SmallSet elt
   removeSmallSet,
      -- :: Ord elt => elt -> SmallSet elt -> SmallSet elt
      -- removeSmallSet should never be called if the element
      -- is not in the set.
   listSmallSet,
      -- :: Ord elt => SmallSet elt -> [elt]
   minusSmallSet
      -- :: Ord elt => SmallSet elt -> SmallSet elt -> SmallSet elt
      -- returns nodes in first set but not in second
   ) where

import ExtendedPrelude

------------------------------------------------------------------------
-- We use lists because Haskell sets carry too much
-- baggage.  There are probably many better ways.
-- However we do specify that the lists are ordered.  Since
-- people will often add elements in the order of generation, we order
-- them so that the least comes last in the list (the opposite
-- way to what you might expect).
------------------------------------------------------------------------

newtype Ord elt => SmallSet elt = SmallSet [elt]

emptySmallSet :: Ord elt => SmallSet elt
emptySmallSet = SmallSet []

addSmallSet :: Ord elt => elt -> SmallSet elt -> SmallSet elt
addSmallSet elt (SmallSet eltList) =
   SmallSet (ExtendedPrelude.insertOrdGt elt eltList)

addSmallSetList :: Ord elt => [elt] -> SmallSet elt -> SmallSet elt
addSmallSetList eltList smallSet =
   foldr
      addSmallSet
      smallSet
      eltList

removeSmallSet :: Ord elt => elt -> SmallSet elt -> SmallSet elt
removeSmallSet elt (SmallSet eltList) =
      SmallSet (remove eltList)
   where
      remove (first:rest) =
      -- pattern match error here means removeSmallSet is called
      -- on an element which isn't in the set.
         if first == elt
            then
               rest
            else
               first:(remove rest)
        

listSmallSet :: Ord elt => SmallSet elt -> [elt]
listSmallSet (SmallSet eltList) = eltList

minusSmallSet :: Ord elt => SmallSet elt -> SmallSet elt -> SmallSet elt
minusSmallSet (SmallSet eltList1) (SmallSet eltList2) =
      SmallSet (subtract eltList1 eltList2)
   where
      subtract eltList1 [] = eltList1
      subtract [] eltList2 = []
      subtract eltList1@(first1:rest1) eltList2@(first2:rest2) =
         case compare first1 first2 of
            LT -> first1:(subtract rest1 


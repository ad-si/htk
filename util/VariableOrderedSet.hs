{- A VariableOrderedSet is like a VariableSet except in two ways.
   (1) the elements are ordered.  Elements always come out in the same order
       in which they are provided.
   (2) multiple copies of the same element are permitted.  Deleting
       simply deletes one of the copies (which is unspecified).
   -}
module VariableOrderedSet(
   VariableOrderedSet,
   newEmptyVariableOrderedSet,
   newVariableOrderedSet,
   updateOrderedSet,
   setVariableOrderedSet,
   ) where

import Dynamics
import Sink
import Broadcaster
import VariableSet

-- --------------------------------------------------------------------
-- The types
-- --------------------------------------------------------------------

---
-- The elements themselves are stored in reverse order.
newtype VariableOrderedSetData x = VariableOrderedSetData [Keyed x]

newtype VariableOrderedSet x =
   VariableOrderedSet (Broadcaster (VariableOrderedSetData x) 
      (VariableSetUpdate x))


-- --------------------------------------------------------------------
-- The functions
-- --------------------------------------------------------------------

unKey :: Keyed x -> x
unKey (Keyed x) = x


update :: HasKey x key => VariableOrderedSetData x -> VariableSetUpdate x ->
   Maybe (VariableOrderedSetData x)
update (VariableOrderedSetData list) update =
   case update of
      AddElement x ->
         Just (VariableOrderedSetData (Keyed x:list))
      DelElement x ->
         let
            kx = Keyed x
            scan [] = Nothing
            scan (ky:rest) = 
               if ky == kx
                  then
                     Just rest
                  else
                     fmap (ky:) (scan rest)
         in
            fmap VariableOrderedSetData (scan list)


-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------


---
-- Create a new empty variable ordered set.
newEmptyVariableOrderedSet :: HasKey x key => IO (VariableOrderedSet x)
newEmptyVariableOrderedSet = 
   do
      broadcaster <- newGeneralBroadcaster update (VariableOrderedSetData [])
      return (VariableOrderedSet broadcaster)

---
-- Create a new variable set with given contents
newVariableOrderedSet :: HasKey x key => [x] -> IO (VariableOrderedSet x)
newVariableOrderedSet contents =
   do
      broadcaster <- newGeneralBroadcaster update 
         (VariableOrderedSetData (reverse (map Keyed contents)))
      return (VariableOrderedSet broadcaster)

---
-- Update a variable set in some way.
updateOrderedSet :: HasKey x key => VariableOrderedSet x 
  -> VariableSetUpdate x -> IO ()
updateOrderedSet (VariableOrderedSet broadcaster) update = 
   updateBroadcaster broadcaster update

---
-- Set the elements of the variable set.
setVariableOrderedSet :: HasKey x key => VariableOrderedSet x -> [x] -> IO ()
setVariableOrderedSet (VariableOrderedSet broadcaster) newList =
   do
     let
        updateFn (VariableOrderedSetData oldList) =
           let
              updates = 
                 (map DelElement (map unKey oldList)) 
                    ++ (map AddElement newList)
           in
              (VariableOrderedSetData (reverse (map Keyed newList)),updates)

     -- try to avoid lengthy evaluations of newList while broadcaster is
     -- locked.
     seq (length newList) (anyUpdateBroadcaster broadcaster updateFn)


-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------

instance HasKey x key 
   => CanAddSinks (VariableOrderedSet x) [x] (VariableSetUpdate x) where
   addOldSink (VariableOrderedSet broadcaster) sink =
      do
         (VariableOrderedSetData list) <- addOldSink broadcaster sink
         return (map unKey (reverse list))

   readContents (VariableOrderedSet broadcaster) =
      do
         (VariableOrderedSetData list) <- readContents broadcaster
         return (map unKey (reverse list))

-- --------------------------------------------------------------------
-- Make VariableOrderedSet Typeable
-- --------------------------------------------------------------------

variableOrderedSet_tyRep = mkTyRep "VariableOrderedSet" "VariableOrderedSet"
instance HasTyRep1 VariableOrderedSet where
   tyRep1 _ = variableOrderedSet_tyRep


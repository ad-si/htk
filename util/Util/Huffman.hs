-- | This code does "Huffman" coding, using the queue implementation.  This
-- can be used for constructing Huffman encodings, or for computing factorials
-- efficiently.
module Util.Huffman(
   huffmanFold
   ) where

import Util.Queue

-- | huffmanFold op l
-- where op is associative, l is a nonempty monotonically increasing list,
-- and op has the property that (x1>=x2,y1>=y2) => (op x1 y1>=op x2 y2)
-- computes the fold of l with op, by repeatedly folding the smallest two
-- elements of the list until only one remains.
huffmanFold :: Ord a => (a -> a -> a) -> [a] -> a
huffmanFold op l =
   let
      pointedList = pointList l

      phase1 pointedList =
         case removePointed pointedList of
            Nothing -> error "huffmanFold requires a non-empty list"
            Just (a1,pointedList2) ->
               case removePointed pointedList2 of
                  Nothing -> a1 -- This is already the result of the folding
                  Just (a2,pointedList3) ->
                     case insertAndMovePointer pointedList3 (op a1 a2) of
                        Right pointedList4 -> phase1 pointedList4
                        Left queue -> phase2 queue
      phase2 queue =
         case removeQ queue of
            -- Nothing can't happen
            Just (a1,queue2) ->
               case removeQ queue2 of
                  Just (a2,queue3) ->
                     phase2 (insertQ queue3 (op a1 a2))
                  Nothing -> a1 -- we have a result!
   in
      phase1 pointedList

-- ------------------------------------------------------------------------
-- PointedList operations
-- ------------------------------------------------------------------------

-- | effectively a list with a pointer in the middle which can only be
-- moved right.  The list should always be in increasing order.
data PointedList a = PointedList (Queue a)  [a]

instance Show a => Show (PointedList a) where
   show (PointedList queue l) = show (queueToList queue, l)
-- | pointList makes a new pointed list with the pointer at the left.
pointList :: [a] -> PointedList a
pointList l = PointedList emptyQ l

-- | removePointed gets the first element of a PointedList.  If the pointer
-- is at the start of the list, it is moved to the new head.
removePointed :: PointedList a -> Maybe (a,PointedList a)
removePointed (PointedList queue list) =
   case removeQ queue of
      Nothing ->
         case list of
            [] -> Nothing
            a:list' -> Just (a,PointedList emptyQ list')
      Just (a,queue') -> Just (a,PointedList queue' list)

-- | insertAndMovePointer inserts an element to the right of the pointer,
-- and moves the pointer after it.  It does this maintaining the invariant
-- that the pointed list is ordered, and we assume that all elements to the
-- left of the pointer are not more than the inserted element.
--
-- If the pointer reaches the end of the list, we instead of returning a
-- PointedList, return a queue containing the list contents.
insertAndMovePointer :: Ord a => PointedList a -> a
   -> Either (Queue a) (PointedList a)
insertAndMovePointer (PointedList queue list) a =
   case list of
      [] -> Left (insertQ queue a)
      a2:list' ->
         if a2<a
         then insertAndMovePointer
            (PointedList (insertQ queue a2) list') a
         else
            Right (PointedList (insertQ queue a) list)

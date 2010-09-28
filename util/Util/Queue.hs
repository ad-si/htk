-- | This is an implementation of queues inspired by the paper in
-- Software Practice & Experience, ...
-- The queue is divided into two sequences. The first sequence
-- holds the elements in a LIFO order, the second in a FIFO order.
-- The LIFO sequence is the one where elements are added, the FIFO
-- the one from which elements are removed. When the remove operation
-- is called and the FIFO sequence is empty, the LIFO sequence is
-- turned into a FIFO sequence by reversing the order of its elements.
--
-- Note from GER - as far as I know, we only need the values
--    emptyQ :: Queue a -- new empty queue
--    singletonQ :: a -> Queue a -- new singleton queue
--    insertQ :: Queue a -> a -> Queue a -- add to queue
--    removeQ :: Queue a -> Maybe (a,Queue a) -- pop from queue.
--    insertAtEndQ :: Queue a -> a -> Queue a
--    -- undo the effect of the previous removeQ.
--    isEmptyQ :: Queue a -> Bool
--    queueToList :: Queue a -> [a]
module Util.Queue (
        Queue,

        emptyQ,
        singletonQ,
        isEmptyQ,
        insertQ,
        removeQ,
        insertAtEndQ,

        listToQueue,
        queueToList,
        ) where

-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data Queue a = Queue [a] [a]


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Eq a => Eq (Queue a) where
        (Queue f1 r1) == (Queue f2 r2) =
                (f1 ++ reverse r1) == (f2 ++ reverse r2)

instance Functor Queue where
        fmap f (Queue l1 l2) = Queue (map f l1) (map f l2)

-- --------------------------------------------------------------------------
-- Operations
-- --------------------------------------------------------------------------

emptyQ :: Queue a
emptyQ =  Queue [] []


singletonQ :: a -> Queue a
singletonQ e =  Queue [] [e]


isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue [] []) = True
isEmptyQ _ = False



insertQ :: Queue a -> a -> Queue a
insertQ (Queue fl rl) e = Queue (e:fl) rl



{-

lengthQ :: Queue a -> Int
lengthQ (Queue fl rl) = length fl + length rl

headQ :: Queue a -> a
headQ (Queue fl []) = (head (reverse fl))
headQ (Queue _ rl) = (head rl)

tailQ :: Queue a -> Queue a
tailQ (Queue fl [] ) = Queue [] tl where (_ : tl) = reverse fl
tailQ (Queue fl rl ) = Queue fl (tail rl)


frontQ :: Queue a -> Maybe a
frontQ (Queue [] []) = Nothing
frontQ (Queue fl []) = Just (head (reverse fl))
frontQ (Queue _ rl) = Just (head rl)

-}

removeQ :: Queue a -> Maybe (a, Queue a)
removeQ (Queue [] [] ) = Nothing
-- This function used to return
-- error "removeQ: Queue is empty" where above we have "Nothing".
-- Heaven knows why.  Anyway it's only used in Selective.hs and a
-- test case, so I think I can safely change it.  (GER, 10/2/2000)
removeQ (Queue fl [] ) = Just (x, Queue [] tl) where (x : tl) = reverse fl
removeQ (Queue fl rl ) = Just (head rl, Queue fl (tail rl))

insertAtEndQ :: Queue a -> a -> Queue a
insertAtEndQ (Queue fl rl) next = Queue fl (next:rl)

-- --------------------------------------------------------------------------
-- Converting to and from lists
-- --------------------------------------------------------------------------


-- | Converts a list to a queue with the first element of the list the
-- first element of the queue.
listToQueue :: [a] -> Queue a
listToQueue xs = foldl insertQ emptyQ xs

-- | Inverts listToQueue
queueToList :: Queue a -> [a]
queueToList (Queue fl rl) = rl ++ reverse fl

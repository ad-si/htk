{- #########################################################################

MODULE        : Queue
AUTHOR        : Einar Karlsen, 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : 
        This is an implementation of queues inspired by the paper in 
        Software Practice & Experience, ...
        The queue is divided into two sequences. The first sequence
        holds the elements in a LIFO order, the second in a FIFO order.
        The LIFO sequence is the one where elements are added, the FIFO
        the one from which elements are removed. When the remove operation
        is called and the FIFO sequence is empty, the LIFO sequence is 
        turned into a FIFO sequence by reversing the order of its elements.

   ######################################################################### -}


module Queue (
        Queue,

        emptyQ,
        singletonQ,
        isEmptyQ,
        insertQ,
        headQ,
        tailQ,
        frontQ,
        removeQ,
        lengthQ 
        ) where

import Maybes
import Debug(debug)


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

{-
instance Monad Queue where
        return x = Queue [x] []
        (Queue fl rl) >>= k  = Queue (concat (map k fl)) (concat (map k rl))

instance MonadZero Queue where
        zero = Queue [] []

instance MonadPlus Queue where
        (Queue fx rx) ++ (Queue fy ry) = Queue (fx ++ reverse rx ++ fy) ry
-}

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


lengthQ :: Queue a -> Int
lengthQ (Queue fl rl) = length fl + length rl


insertQ :: Queue a -> a -> Queue a
insertQ (Queue fl rl) e = Queue (e:fl) rl


headQ :: Queue a -> a
headQ (Queue fl []) = (head (reverse fl))
headQ (Queue _ rl) = (head rl)


tailQ :: Queue a -> Queue a
tailQ (Queue fl [] ) = Queue [] tl where (x : tl) = reverse fl
tailQ (Queue fl rl ) = Queue fl (tail rl)


frontQ :: Queue a -> Maybe a
frontQ (Queue [] []) = Nothing
frontQ (Queue fl []) = Just (head (reverse fl))
frontQ (Queue _ rl) = Just (head rl)

removeQ :: Queue a -> Maybe (a, Queue a)
removeQ (Queue [] [] ) = error "removeQ: Queue is empty"
removeQ (Queue fl [] ) = Just (x, Queue [] tl) where (x : tl) = reverse fl
removeQ (Queue fl rl ) = Just (head rl, Queue fl (tail rl))


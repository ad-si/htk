{- #########################################################################

MODULE        : ExtendedPrelude
AUTHOR        : Einar Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2


   ######################################################################### -}


module ExtendedPrelude (
   monadDot,
   split,
   insertOrdLt,
   insertOrdGt,
   insertOrd,
   bottom,
   ) where

import Debug(debug)

-- ---------------------------------------------------------------------------
-- Monad Operations
-- ---------------------------------------------------------------------------

monadDot :: Monad m =>  (b -> m c) -> (a -> m b) -> (a -> m c)
-- The "." operator lifted to monads.   So like ., the arguments
-- are given in the reverse order to that in which they should
-- be executed. 
monadDot f g x =
   do
      y <- g x
      f y

-- ---------------------------------------------------------------------------
-- List Operations
-- ---------------------------------------------------------------------------

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case dropWhile p s of
                [] -> []
                s' -> w : split p s''
                      where (w,s'') = break p s' 

-- ---------------------------------------------------------------------------
-- Ordered List Operations
-- ---------------------------------------------------------------------------

insertOrdLt :: Ord a => a -> [a] -> [a]
insertOrdLt x l = insertOrd (<=) x l

insertOrdGt :: Ord a => a -> [a] -> [a]
insertOrdGt x l = insertOrd (>=) x l

insertOrd :: (a -> a -> Bool) -> a -> [a] -> [a]
insertOrd p x [] = [x]
insertOrd p x ll@(e:l) =
   if p x e
   then
      x : ll
   else
      e : (insertOrd p x l)

-- ---------------------------------------------------------------------------
-- bottom
-- ---------------------------------------------------------------------------

bottom :: a
bottom = error "Attempted to evaluate ExtendedPrelude.bottom"

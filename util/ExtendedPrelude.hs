{- #########################################################################

MODULE        : ExtendedPrelude
AUTHOR        : Einar Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2


   ######################################################################### -}


module ExtendedPrelude (
        split,
        insertOrdLt,
        insertOrdGt,
        insertOrd
        ) where

import Debug(debug)

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
insertOrd p x (e:l) | p e x = e : insertOrd p x l
insertOrd p x l =  x : l


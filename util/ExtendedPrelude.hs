{- #########################################################################

MODULE        : ExtendedPrelude
AUTHOR        : Einar Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2


   ######################################################################### -}


module ExtendedPrelude (
   -- Operations for trimming spaces from Strings.
   trimTrailing,
   trimLeading,
   trimSpaces,

   monadDot,
   split,
   insertOrdLt,
   insertOrdGt,
   insertOrd,
   bottom,

   -- Indicates that this type allows an IO-style map.
   HasMapIO(..),
    ) where

import Char

import Debug(debug)

-- ---------------------------------------------------------------------------
-- Character operations
-- ---------------------------------------------------------------------------

---
-- Remove trailing spaces (We try to avoid reconstructing the string,
-- on the assumption that there aren't often spaces)
trimTrailing :: String -> String
trimTrailing str = 
   case tt str of
      Nothing -> str
      Just str2 -> str2
   where
      tt [] = Nothing
      tt (str@[ch]) = if isSpace ch then Just [] else Nothing
      tt (ch:rest) = 
         case tt rest of
            Nothing -> Nothing
            (j@(Just "")) -> if isSpace ch then j else Just [ch]
            Just trimmed -> Just (ch:trimmed)

---
-- Remove leading spaces
trimLeading :: String -> String
trimLeading [] = []
trimLeading (str@(ch:rest)) = if isSpace ch then trimLeading rest else str

---
-- Remove trailing and leading spaces
trimSpaces :: String -> String
trimSpaces = trimTrailing . trimLeading


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
-- The HasMapIO class
-- ---------------------------------------------------------------------------

class HasMapIO option where
   mapIO :: (a -> IO b) -> option b -> option a

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

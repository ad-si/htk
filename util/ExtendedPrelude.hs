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
   findJust,
   insertOrdLt,
   insertOrdGt,
   insertOrd,
   insertOrdAlternate,
   bottom,

   readCheck,
      -- :: (Read a) => String -> Maybe a
      -- returns Just a if we can read a, and the rest is just spaces.

   chop, -- :: Int -> [a] -> Maybe [a]
      -- removes last elements from a list
   pairList, -- :: a -> [b] -> [(a,b)]
      -- pair of elements of a list.
   lastOpt, -- :: [a] -> Maybe a
      -- gets the last element of a list, safely.

   isPrefix, 
      -- :: Eq a => [a] -> [a] -> Just [a]
      -- returns remainder if the first list is a prefix of the second one.

   -- Indicates that this type allows an IO-style map.
   HasCoMapIO(..),
   HasMapIO(..),
   HasMapMonadic(..),
   mapPartialM,

   splitByChar,
   unsplitByChar,
   unsplitByChar0,
   splitToChar,
   splitToElem,
   splitToElemGeneral,
   deleteFirst,
   deleteFirstOpt,
   deleteAndFindFirst,
   deleteAndFindFirstOpt,
   divideList,

   treeFold,
   treeFoldM,

   mapEq, -- used for instancing Eq
   mapOrd, -- used for instancing Ord.

   BreakFn,
   addFallOut,
   addFallOutWE,

   addSimpleFallOut,
   simpleFallOut,
   mkBreakFn,
   newFallOut,

   addGeneralFallOut,
   GeneralBreakFn(..),GeneralCatchFn(..),
   catchOurExceps, -- :: IO a -> IO (Either String a) 
   errorOurExceps, -- :: IO a -> IO a
   ourExcepToMess, -- :: Exception -> Maybe String
   breakOtherExceps, -- :: BreakFn -> IO a -> IO a

   EqIO(..),OrdIO(..),

   uniqOrd,
   uniqOrdOrder,
   allSame,

   fmToList_GE_1,
   maxFM_1,
   keysFM_GE_1,
   ) where

import Char
import Monad
import Maybe
import qualified Data.FiniteMap

import Data.Set
import Control.Exception
import System.IO.Unsafe

import Object
import Debug(debug)
import Computation
import Dynamics

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


---
-- returns Just a if we can read a, and the rest is just spaces.
readCheck :: Read a => String -> Maybe a
readCheck str = case reads str of
   [(val,s)] | all isSpace s  -> Just val
   _ -> Nothing


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
-- Things to do with maps
-- ---------------------------------------------------------------------------

class HasMapIO option where
   mapIO :: (a -> IO b) -> option a -> option b

class HasCoMapIO option where
   coMapIO :: (a -> IO b) -> option b -> option a

class HasMapMonadic h where
   mapMonadic :: Monad m => (a -> m b) -> h a -> m (h b)

instance HasMapMonadic [] where
   mapMonadic = mapM

mapPartialM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapPartialM mapFn as =
   do
      bOpts <- mapM mapFn as
      return (catMaybes bOpts)
{-# SPECIALIZE mapPartialM :: (a -> IO (Maybe b)) -> [a] -> IO [b] #-}

-- ---------------------------------------------------------------------------
-- List Operations
-- ---------------------------------------------------------------------------

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case dropWhile p s of
                [] -> []
                s' -> w : split p s''
                      where (w,s'') = break p s' 

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f [] = Nothing
findJust f (x:xs) = case f x of
   (y@ (Just _)) -> y
   Nothing -> findJust f xs

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst fn [] = error "ExtendedPrelude.deleteFirst - not found"
deleteFirst fn (a:as) = 
   if fn a then as else a:deleteFirst fn as

deleteFirstOpt :: (a -> Bool) -> [a] -> [a]
deleteFirstOpt fn as = case deleteAndFindFirstOpt fn as of
   Nothing -> as
   Just (_,as) -> as

deleteAndFindFirst :: (a -> Bool) -> [a] -> (a,[a])
deleteAndFindFirst fn [] 
   = error "ExtendedPrelude.deleteAndFindFirst - not found"
deleteAndFindFirst fn (a:as) =
   if fn a then (a,as) else 
      let 
         (a1,as1) = deleteAndFindFirst fn as
      in
         (a1,a:as1)

deleteAndFindFirstOpt :: (a -> Bool) -> [a] -> Maybe (a,[a])
deleteAndFindFirstOpt fn [] = Nothing
deleteAndFindFirstOpt fn (a:as) =
   if fn a then Just (a,as) else
      fmap
         (\ (a1,as1) -> (a1,a:as1))
         (deleteAndFindFirstOpt fn as)

divideList :: (a -> Either b c) -> [a] -> ([b],[c])
divideList fn [] = ([],[])
divideList fn (a:as) =
   let
      (bs,cs) = divideList fn as
   in
      case fn a of
         Left b -> (b:bs,cs)
         Right c -> (bs,c:cs)


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


---
-- insertOrdAlternate is similar to insertOrd except (1) it takes an Ordering
-- argument; (2) if it finds an argument that matches, it applies the
-- given function to generate a new element, rather than inserting another.
-- The new generated element should be EQ to the old one.
insertOrdAlternate :: (a -> a -> Ordering) -> a -> (a -> a) -> [a] -> [a]
insertOrdAlternate p x merge [] = [x]
insertOrdAlternate p x merge (ll@(e:l)) =
   case p x e of
      LT -> x : ll
      EQ -> merge e : l
      GT -> e : insertOrdAlternate p x merge l

-- ---------------------------------------------------------------------------
-- bottom
-- ---------------------------------------------------------------------------

bottom :: a
bottom = error "Attempted to evaluate ExtendedPrelude.bottom"


-- ---------------------------------------------------------------------------
-- Splitting a string up into a list of strings and unsplitting back
-- by a single character.
-- Examples:
--    splitByChar '.' "a.b.." = ["a","b","",""]
--    splitByChar '.' "" = [""]
-- unsplitByChar is the inverse function.
-- unsplitByChar0 allows the empty list.
-- ---------------------------------------------------------------------------

splitByChar :: Char -> String -> [String]
splitByChar ch s = split s
   where
      split s = case splitTo s of
         Nothing -> [s]
         Just (s1,s2) -> s1 : split s2

      splitTo [] = Nothing
      splitTo (c:cs) = if c == ch then Just ([],cs) else
         fmap
            (\ (cs1,cs2) -> (c:cs1,cs2))
            (splitTo cs)

unsplitByChar :: Char -> [String] -> String
unsplitByChar ch [] = error "unsplitByChar not defined for empty list"
unsplitByChar ch l = foldr1 (\w s -> w ++ ch:s) l

unsplitByChar0 :: Char -> [String] -> String
unsplitByChar0 ch [] = ""
unsplitByChar0 ch l = unsplitByChar ch l

-- ------------------------------------------------------------------------
-- Splitting to and after a character
-- ------------------------------------------------------------------------

---
-- We split at the first occurrence of the character, returning the
-- string before and after.
splitToChar :: Char -> String -> Maybe (String,String)
splitToChar c = sTC
   where
      sTC [] = Nothing
      sTC (x:xs) = 
         if x == c then Just ([],xs) else 
            fmap
               (\ (xs1,xs2) -> (x:xs1,xs2))
               (sTC xs)

-- ------------------------------------------------------------------------
-- Like splitToChar, but with an arbitrary predicate.
-- ------------------------------------------------------------------------

splitToElem :: (a -> Bool) -> [a] -> Maybe ([a],[a])
splitToElem fn = sTC
   where
      sTC [] = Nothing
      sTC (x:xs) = 
         if fn x then Just ([],xs) else 
            fmap
               (\ (xs1,xs2) -> (x:xs1,xs2))
               (sTC xs)

-- ------------------------------------------------------------------------
-- Like splitToElem, but also return the matching element
-- ------------------------------------------------------------------------

splitToElemGeneral :: (a -> Bool) -> [a] -> Maybe ([a],a,[a])
splitToElemGeneral fn = sTC
   where
      sTC [] = Nothing
      sTC (x:xs) = 
         if fn x then Just ([],x,xs) else 
            fmap
               (\ (xs1,x1,xs2) -> (x:xs1,x1,xs2))
               (sTC xs)

-- ------------------------------------------------------------------------
-- Removing the last n elements from a list
-- ------------------------------------------------------------------------

chop :: Int -> [a] -> Maybe [a]
chop n list =
   let
      toTake = length list - n
   in
      if toTake >=0 then Just (take toTake list) else Nothing

-- ------------------------------------------------------------------------
-- Pair off elements of a list
-- ------------------------------------------------------------------------

pairList :: a -> [b] -> [(a,b)]
pairList a bs = map (\ b -> (a,b)) bs

-- ------------------------------------------------------------------------
-- Get the last element (safely)
-- ------------------------------------------------------------------------

lastOpt :: [a] -> Maybe a
lastOpt [] = Nothing
lastOpt [a] = Just a
lastOpt (_:rest) = lastOpt rest


-- ------------------------------------------------------------------------
-- Prefix functions
-- ------------------------------------------------------------------------



-- | returns remainder if the first list is a prefix of the second one.
isPrefix :: Eq a => [a] -> [a] -> Maybe [a]
isPrefix [] s = Just s
isPrefix (c1 : c1s) (c2 : c2s) | c1 == c2 
   = isPrefix c1s c2s
isPrefix _ _ = Nothing

-- ------------------------------------------------------------------------
-- Folding a Tree
-- ------------------------------------------------------------------------

---
-- node is the tree's node type.
-- state is folded through every node of the tree (and is the result).
-- We search the tree in depth-first order, applying visitNode at each
--   node to update the state.
-- The ancestorInfo information comes from the ancestors of the node.  EG
-- if we are visiting node N1 which came from N2 the ancestorInfo given to
-- visitNode for N1 will be that computed from visitNode for N2. 
-- For the root node, it will be initialAncestor
treeFold :: 
   (ancestorInfo -> state -> node -> (ancestorInfo,state,[node])) 
   -> ancestorInfo -> state -> node 
   -> state
treeFold visitNode initialAncestor initialState node =
   let
      (newAncestor,newState,children) 
         = visitNode initialAncestor initialState node
   in
      foldl
         (\ state node -> treeFold visitNode newAncestor state node)
         newState
         children

---
-- Like treeFold, but using monads.
treeFoldM :: Monad m =>
   (ancestorInfo -> state -> node -> m (ancestorInfo,state,[node])) 
   -> ancestorInfo -> state -> node 
   -> m state
treeFoldM visitNode initialAncestor initialState node =
   do
      (newAncestor,newState,children) 
         <- visitNode initialAncestor initialState node
      foldM
         (\ state node -> treeFoldM visitNode newAncestor state node)
         newState
         children

-- ------------------------------------------------------------------------
-- Functions which make it easy to create new instances of Eq and Ord.
-- ------------------------------------------------------------------------

---
-- Produce an equality function for b
mapEq :: Eq a => (b -> a) -> (b -> b -> Bool)
mapEq toA b1 b2 = (toA b1) == (toA b2)

---
-- Produce a compare function for b
mapOrd :: Ord a => (b -> a) -> (b -> b -> Ordering)
mapOrd toA b1 b2 = compare (toA b1) (toA b2)

-- ------------------------------------------------------------------------
-- Adding fall-out actions to IO actions
-- ------------------------------------------------------------------------

---
-- A function indicating we want to escape from the current computation.
type BreakFn = (forall other . String -> other)

---
-- Intended use, EG
--    addFallOut (\ break ->
--       do
--          -- blah blah (normal IO a stuff) --
--          when (break condition)  
--             (break "You can't do that there ere")
--          -- more blah blah, not executed if there's an break --
--          return (value of type a)
--       )
addFallOut :: (BreakFn -> IO a) -> IO (Either String a)
addFallOut getAct =
   do
      (id,tryFn) <- newFallOut
      tryFn (getAct (mkBreakFn id))

---
-- Like addFallOut, but returns a WithError object instead.
addFallOutWE :: (BreakFn -> IO a) -> IO (WithError a)
addFallOutWE toAct =
   do
      result <- addFallOut toAct
      return (toWithError result)

            
simpleFallOut :: BreakFn
simpleFallOut = mkBreakFn simpleFallOutId

addSimpleFallOut :: IO a -> IO (Either String a)


(simpleFallOutId,addSimpleFallOut) = mkSimpleFallOut

mkSimpleFallOut = unsafePerformIO newFallOut
{-# NOINLINE mkSimpleFallOut #-}

data FallOutExcep = FallOutExcep {
   fallOutId :: ObjectID,
   mess :: String
   } deriving (Typeable)

mkBreakFn :: ObjectID -> BreakFn
mkBreakFn id mess = throwDyn (FallOutExcep {fallOutId = id,mess = mess})


newFallOut :: IO (ObjectID,IO a -> IO (Either String a))
newFallOut =
   do
      id <- newObject
      let
         tryFn act =
            tryJust
               (\ exception -> case dynExceptions exception of
                  Nothing -> Nothing 
                     -- don't handle this as it's not even a dyn.
                  Just dyn ->
                     case fromDyn dyn of
                        Nothing -> Nothing -- not a fallout.
                        Just fallOutExcep -> if fallOutId fallOutExcep /= id
                           then
                              Nothing 
                              -- don't handle this; it's from another 
                              -- addFallOut
                           else
                              Just (mess fallOutExcep)
                  )
               act

      return (id,tryFn)

-- ------------------------------------------------------------------------
-- More general try/catch function.
-- ------------------------------------------------------------------------

data GeneralBreakFn a = GeneralBreakFn (forall b . a -> b)
data GeneralCatchFn a = GeneralCatchFn (forall c . IO c -> IO (Either a c))

addGeneralFallOut :: Typeable a => IO (GeneralBreakFn a,GeneralCatchFn a)
addGeneralFallOut =
   do
      (objectId,catchFn) <- newGeneralFallOut
      let
         breakFn a = throwDyn (GeneralFallOutExcep {
            generalFallOutId = objectId,a=a})
      return (GeneralBreakFn breakFn,catchFn)


data GeneralFallOutExcep a = GeneralFallOutExcep {
   generalFallOutId :: ObjectID,
   a :: a
   } deriving (Typeable)

newGeneralFallOut :: Typeable a => IO (ObjectID,GeneralCatchFn a)
newGeneralFallOut =
   do
      id <- newObject
      let
         tryFn act =
            tryJust
               (\ exception -> case dynExceptions exception of
                  Nothing -> Nothing 
                     -- don't handle this as it's not even a dyn.
                  Just dyn ->
                     case fromDyn dyn of
                        Nothing -> Nothing 
                           -- not a fallout, or not the right type of a.
                        Just generalFallOutExcep -> 
                              if generalFallOutId generalFallOutExcep /= id
                           then
                              Nothing 
                              -- don't handle this; it's from another 
                              -- addGeneralFallOut
                           else
                              Just (a generalFallOutExcep)
                  )
               act

      return (id,GeneralCatchFn tryFn)

-- ------------------------------------------------------------------------
-- General catch function for our exceptions.
-- ------------------------------------------------------------------------

ourExcepToMess :: Exception -> Maybe String
ourExcepToMess excep = case dynExceptions excep of
   Nothing -> Nothing
   Just dyn -> 
      case fromDyn dyn of
         Just fallOut -> Just ("Fall-out exception " 
            ++ show (fallOutId fallOut) ++ ": " ++ mess fallOut)
         Nothing -> Just ("Mysterious dynamic exception " ++ show dyn)

catchOurExceps :: IO a -> IO (Either String a) 
catchOurExceps act =
   tryJust ourExcepToMess act

errorOurExceps :: IO a -> IO a
errorOurExceps act =
   do
      eOrA <- catchOurExceps act
      case eOrA of
         Left mess -> error mess
         Right a -> return a

breakOtherExceps :: BreakFn -> IO a -> IO a
breakOtherExceps break act =
   catchJust
      (\ excep -> if isJust (ourExcepToMess excep) 
         then
            Nothing
         else
            Just (break ("Haskell Exception: " ++ show excep))
         )
      act
      id
            



-- ------------------------------------------------------------------------
-- Where equality and comparing requires IO.
-- ------------------------------------------------------------------------

class EqIO v where
   eqIO :: v -> v -> IO Bool

class EqIO v => OrdIO v where
   compareIO :: v -> v -> IO Ordering

-- ------------------------------------------------------------------------
-- Eq/Ord operations
-- ------------------------------------------------------------------------

-- ¦ Remove duplicate elements from a list.
uniqOrd :: Ord a => [a] -> [a]
uniqOrd = setToList . mkSet

-- ¦ Like uniqOrd, except that we specify the output order of the list.
-- The resulting list is that obtained by deleting all duplicate elements
-- in the list, except the first, for example [1,2,3,2,1,4] will go to
-- [1,2,3,4].
uniqOrdOrder :: Ord a => [a] -> [a]
uniqOrdOrder list = mkList emptySet list
   where
      mkList _ [] = []
      mkList set (a : as) =
         if elementOf a set
            then
               mkList set as
            else
               a : mkList (addToSet set a) as

-- | Return Just True if all the elements give True, Just False if all False,
-- Nothing otherwise (or list is empty).
allSame :: (a -> Bool) -> [a] -> Maybe Bool
allSame fn [] = Nothing
allSame fn (a : as) =
   if fn a
      then
         if all fn as
            then
               Just True
            else
               Nothing
      else
         if any fn as
            then
               Nothing
            else
               Just False  

-- ------------------------------------------------------------------------
-- Operations on FiniteMaps
-- These will hopefully be in GHC6.1 onwards anyway.
-- ------------------------------------------------------------------------

fmToList_GE_1 :: Ord key 
   => Data.FiniteMap.FiniteMap key elt -> key ->  [(key,elt)]
maxFM_1 :: Ord key => Data.FiniteMap.FiniteMap key elt -> Maybe key
keysFM_GE_1 :: Ord key => Data.FiniteMap.FiniteMap key elt -> key -> [key]


#if (__GLASGOW_HASKELL__ >= 610)
fmToList_GE_1 = Data.FiniteMap.fmToList_GE
maxFM_1 = Data.FiniteMap.maxFM
keysFM_GE_1 = Data.FiniteMap.keysFM_GE
#else
-- stopgap solutions
fmToList_GE_1 map key =
   filter
      (\ (key1,_) -> key1 >= key)
      (Data.FiniteMap.fmToList map)

maxFM_1 map =
   case Data.FiniteMap.keysFM map of
      [] -> Nothing
      keys -> Just (last keys)

keysFM_GE_1 fm key  = map fst (fmToList_GE_1 fm key)
#endif


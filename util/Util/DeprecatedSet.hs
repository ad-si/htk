-----------------------------------------------------------------------------
-- |
-- Module      :  DeprecatedSet
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style
--
-- Maintainer  :  maeder@tzi.de
-- Stability   :  deprecated
-- Portability :  portable
-----------------------------------------------------------------------------

module Util.DeprecatedSet (
        -- * Set type
        Set,          -- instance Eq,Ord,Show,Read,Data,Typeable
        union,        -- identical in Data.Set
        -- * Old deprecated interface
        emptySet,       -- :: Set a
        mkSet,          -- :: Ord a => [a]  -> Set a
        setToList,      -- :: Set a -> [a]
        unitSet,        -- :: a -> Set a
        elementOf,      -- :: Ord a => a -> Set a -> Bool
        isEmptySet,     -- :: Set a -> Bool
        cardinality,    -- :: Set a -> Int
        unionManySets,  -- :: Ord a => [Set a] -> Set a
        minusSet,       -- :: Ord a => Set a -> Set a -> Set a
        mapSet,         -- :: Ord a => (b -> a) -> Set b -> Set a
        intersect,      -- :: Ord a => Set a -> Set a -> Set a
        addToSet,       -- :: Ord a => Set a -> a -> Set a
        delFromSet,     -- :: Ord a => Set a -> a -> Set a
        ) where

import Data.Set
    ( Set, empty, fromList, elems, singleton, member, null, size, unions
    , difference, map, intersection, insert, delete, union )

{--------------------------------------------------------------------
  Old Data.Set compatibility interface
--------------------------------------------------------------------}

-- | Obsolete equivalent of 'empty'.
emptySet :: Set a
emptySet = empty

-- | Obsolete equivalent of 'fromList'.
mkSet :: Ord a => [a]  -> Set a
mkSet = fromList

-- | Obsolete equivalent of 'elems'.
setToList :: Set a -> [a]
setToList = elems

-- | Obsolete equivalent of 'singleton'.

unitSet :: a -> Set a
unitSet = singleton

-- | Obsolete equivalent of 'member'.
elementOf :: Ord a => a -> Set a -> Bool
elementOf = member

-- | Obsolete equivalent of 'Data.Set.null'.
isEmptySet :: Set a -> Bool
isEmptySet = Data.Set.null

-- | Obsolete equivalent of 'size'.
cardinality :: Set a -> Int
cardinality = size

-- | Obsolete equivalent of 'unions'.
unionManySets :: Ord a => [Set a] -> Set a
unionManySets = unions

-- | Obsolete equivalent of 'difference'.
minusSet :: Ord a => Set a -> Set a -> Set a
minusSet = difference

-- | Obsolete equivalent of 'Data.Set.map'.
mapSet :: (Ord a, Ord b) => (b -> a) -> Set b -> Set a
mapSet = Data.Set.map

-- | Obsolete equivalent of 'intersection'.
intersect :: Ord a => Set a -> Set a -> Set a
intersect = intersection

-- | Obsolete equivalent of @'flip' 'insert'@.
addToSet :: Ord a => Set a -> a -> Set a
addToSet = flip insert

-- | Obsolete equivalent of @'flip' 'delete'@.
delFromSet :: Ord a => Set a -> a -> Set a
delFromSet = flip delete

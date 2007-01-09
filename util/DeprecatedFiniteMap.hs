-----------------------------------------------------------------------------
-- |
-- Module      :  DeprecatedFiniteMap
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated
-- Portability :  portable
--
-- NOTE: This is a re-implementation of DeprecatedFiniteMap,
--         which has been DEPRECATED and removed from the main branch.
--       It retains the old API, but uses the newer Data.Map implementation.
--
-----------------------------------------------------------------------------

module DeprecatedFiniteMap
  (
        -- * The @FiniteMap@ type
        FiniteMap,              -- abstract type

        -- * Construction
        emptyFM, unitFM, listToFM,

        -- * Lookup operations
        lookupFM, lookupWithDefaultFM,
        elemFM,

        -- * Adding elements
        addToFM,
        addToFM_C,
        addListToFM,
        addListToFM_C,

        -- * Deleting elements
        delFromFM,
        delListFromFM,

        -- * Combination
        plusFM,
        plusFM_C,

        -- * Extracting information
        fmToList, keysFM, eltsFM,
        sizeFM, isEmptyFM,

        -- * Other operations
        minusFM,
        foldFM,
        intersectFM,
        intersectFM_C,
        mapFM, filterFM,
        foldFM_LE, fmToList_LE, keysFM_LE,
        foldFM_GE, fmToList_GE, keysFM_GE,
        minFM, maxFM,
    ) where

import qualified Data.Map as Map        -- the real implementation


-- ---------------------------------------------------------------------------
-- The signature of the module

-- | A mapping from @key@s to @elt@s.
type FiniteMap key elt = Map.Map key elt

-- | An empty 'FiniteMap'.
emptyFM         :: FiniteMap key elt
emptyFM         = Map.empty

-- | A 'FiniteMap' containing a single mapping
unitFM          :: key -> elt -> FiniteMap key elt
unitFM          = Map.singleton

-- | Makes a 'FiniteMap' from a list of @(key,value)@ pairs. In the
-- case of duplicates, the last is taken
listToFM        :: (Ord key) => [(key,elt)] -> FiniteMap key elt
listToFM        = Map.fromList


--      ADDING AND DELETING

-- | Adds an element to a 'FiniteMap'.  Any previous mapping with the same
-- key is overwritten.
addToFM         :: (Ord key) => FiniteMap key elt -> key -> elt  -> FiniteMap key elt
addToFM fm k e  = Map.insert k e fm

-- | Adds a list of elements to a 'FiniteMap', in the order given in
-- the list.  Overwrites previous mappings.
addListToFM     :: (Ord key) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt
addListToFM     = foldr (uncurry Map.insert)

                   -- Combines with previous binding
                   -- In the combining function, the first argument is the "old" element,
                   -- while the second is the "new" one.

-- | Adds an element to a 'FiniteMap'.  If there is already an element
-- with the same key, then the specified combination function is used
-- to calculate the new value. The already present element is passed as
-- the first argument and the new element to add as second.
addToFM_C       :: (Ord key) => (elt -> elt -> elt)
                           -> FiniteMap key elt -> key -> elt
                           -> FiniteMap key elt
addToFM_C c fm k e      = Map.insertWith c k e fm

-- | A list version of 'addToFM_C'.  The elements are added in the
-- order given in the list.
addListToFM_C   :: (Ord key) => (elt -> elt -> elt)
                           -> FiniteMap key elt -> [(key,elt)]
                           -> FiniteMap key elt
addListToFM_C c = foldr (uncurry (Map.insertWith c))

-- | Deletes an element from a 'FiniteMap'.  If there is no element with
-- the specified key, then the original 'FiniteMap' is returned.
delFromFM       :: (Ord key) => FiniteMap key elt -> key   -> FiniteMap key elt
delFromFM       = flip Map.delete

-- | List version of 'delFromFM'.
delListFromFM   :: (Ord key) => FiniteMap key elt -> [key] -> FiniteMap key elt
delListFromFM   = foldr Map.delete

-- | Combine two 'FiniteMap's.  Mappings in the second argument shadow
-- those in the first.
plusFM          :: (Ord key) => FiniteMap key elt -> FiniteMap key elt
                           -> FiniteMap key elt
plusFM          = flip Map.union

-- | Combine two 'FiniteMap's.  The specified combination function is
-- used to calculate the new value when there are two elements with
-- the same key.
plusFM_C        :: (Ord key) => (elt -> elt -> elt)
                           -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
plusFM_C        = Map.unionWith

-- | @(minusFM a1 a2)@ deletes from @a1@ any mappings which are bound in @a2@
minusFM         :: (Ord key) => FiniteMap key elt1 -> FiniteMap key elt2 -> FiniteMap key elt1
minusFM         = Map.difference        -- not sure if this is correct

-- | @(intersectFM a1 a2)@ returns a new 'FiniteMap' containing
-- mappings from @a1@ for which @a2@ also has a mapping with the same
-- key.
intersectFM     :: (Ord key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersectFM     = Map.intersection

-- | Returns the intersection of two mappings, using the specified
-- combination function to combine values.
intersectFM_C   :: (Ord key) => (elt1 -> elt2 -> elt3)
                           -> FiniteMap key elt1 -> FiniteMap key elt2 -> FiniteMap key elt3
intersectFM_C   = Map.intersectionWith

--      MAPPING, FOLDING, FILTERING
foldFM          :: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
foldFM          = Map.foldWithKey
mapFM           :: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
mapFM           = Map.mapWithKey
filterFM        :: (Ord key) => (key -> elt -> Bool)
                           -> FiniteMap key elt -> FiniteMap key elt
filterFM        = Map.filterWithKey

--      INTERROGATING
sizeFM          :: FiniteMap key elt -> Int
sizeFM          = Map.size
isEmptyFM       :: FiniteMap key elt -> Bool
isEmptyFM       = Map.null

-- | Returns 'True' if the specified @key@ has a mapping in this
-- 'FiniteMap', or 'False' otherwise.
elemFM          :: (Ord key) => key -> FiniteMap key elt -> Bool
elemFM          = Map.member

-- | Looks up a key in a 'FiniteMap', returning @'Just' v@ if the key
-- was found with value @v@, or 'Nothing' otherwise.
lookupFM        :: (Ord key) => FiniteMap key elt -> key -> Maybe elt
lookupFM        = flip Map.lookup

-- | Looks up a key in a 'FiniteMap', returning @elt@ if the specified
-- @key@ was not found.
lookupWithDefaultFM
                :: (Ord key) => FiniteMap key elt -> elt -> key -> elt
                -- lookupWithDefaultFM supplies a "default" elt
                -- to return for an unmapped key
lookupWithDefaultFM fm e k
                = Map.findWithDefault e k fm

--      LISTIFYING

-- | Convert a 'FiniteMap' to a @[(key, elt)]@ sorted by 'Ord' key
--
fmToList        :: FiniteMap key elt -> [(key,elt)]
fmToList        = Map.assocs

-- | Extract the keys from a 'FiniteMap', in the order of the keys, so
--
-- > keysFM == map fst . fmToList
--
keysFM          :: FiniteMap key elt -> [key]
keysFM          = Map.keys

-- | Extract the elements from a 'FiniteMap', in the order of the keys, so
--
-- > eltsFM == map snd . fmToList
--
eltsFM          :: FiniteMap key elt -> [elt]
eltsFM          = Map.elems

minFM           :: Ord key => FiniteMap key elt -> Maybe key
maxFM           :: Ord key => FiniteMap key elt -> Maybe key
minFM fm | isEmptyFM fm = Nothing
         | otherwise    = Just (fst (Map.findMin fm))
maxFM fm | isEmptyFM fm = Nothing
         | otherwise    = Just (fst (Map.findMax fm))
-- ---------------------------------------------------------------------------

-- | List elements greater than or equal to the supplied key, in decreasing
-- order
fmToList_LE      :: Ord key => FiniteMap key elt -> key ->  [(key,elt)]
fmToList_LE fm fr = foldFM_LE (\ key elt rest -> (key,elt) : rest) [] fr fm

-- | List keys greater than or equal to the supplied key, in decreasing order
keysFM_LE       :: Ord key => FiniteMap key elt -> key -> [key]
keysFM_LE fm fr  = foldFM_LE (\ key elt rest -> key : rest)       [] fr fm

-- | Fold through all elements less than or equal to the supplied key,
-- in decreasing order.
foldFM_LE       :: Ord key => (key -> elt -> a -> a) -> a -> key ->
   FiniteMap key elt -> a
foldFM_LE f v k = Map.foldWithKey (\ i w c -> if i > k then c else f i w c) v

-- | Fold through all elements greater than or equal to the supplied key,
-- in increasing order.
foldFM_GE       :: Ord key => (key -> elt -> a -> a) -> a -> key ->
   FiniteMap key elt -> a
foldFM_GE f v k = Map.foldWithKey (\ i w c -> if i < k then c else f i w c) v

-- | List elements greater than or equal to the supplied key, in increasing
-- order
fmToList_GE      :: Ord key => FiniteMap key elt -> key ->  [(key,elt)]
fmToList_GE fm fr = foldFM_GE (\ key elt rest -> (key,elt) : rest) [] fr fm

-- | List keys greater than or equal to the supplied key, in increasing order
keysFM_GE       :: Ord key => FiniteMap key elt -> key -> [key]
keysFM_GE fm fr  = foldFM_GE (\ key elt rest -> key : rest)       [] fr fm

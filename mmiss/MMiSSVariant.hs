{- This module controls selection by variant attributes.

   The problem we solve is search a database MMiSSVariantDict a
   of a-values indexed by partial maps from variantAttributes strings
   (listed in the variable of the same name) to Strings.

   When searching by a key (such a map), an item in the database
      indexed by another key (such a map) matches if the two maps
      agree wherever they are both defined.  Thus the null-value
      corresponds to a wildcard.
   Where more than one item in the database is defined, we choose
      some item in the database for which the domain of its map
      is maximal.
  -}
module MMiSSVariant(
   MMiSSVariantDict,
   newEmptyVariantDict,
   variantDictSearch,
   addToVariantDict,
   ) where

import Maybe

import Array
import Concurrent

import ExtendedPrelude
import Dynamics
import Registry

import CodedValue
import BasicObjects

---
-- variantAttributes is the list of all attributes to be classed as
-- variant.  
--
-- Hopefully this list will eventually be worked out from a DTD rather
-- than hardwired into the source code.
variantAttributes :: [String]
variantAttributes = ["language","formalism","level-of-detail",
   "interaction-level"]

variantAttributesArray :: Array Int String
variantAttributesArray = 
   listArray (1,length variantAttributes) variantAttributes

-- -------------------------------------------------------------------
-- The impure interface
-- -------------------------------------------------------------------

newtype MMiSSVariantDict a = MMiSSVariantDict (MVar (VariantDict a))

newEmptyVariantDict :: IO (MMiSSVariantDict a)
newEmptyVariantDict = 
   do
      mVar <- newMVar emptyVariantDict
      return (MMiSSVariantDict mVar)

variantDictSearch :: MMiSSVariantDict a -> Attributes -> 
   IO (Maybe a)
variantDictSearch (MMiSSVariantDict mVar) attributes =
   do
      dict <- readMVar mVar
      search dict (getValueOpt attributes)

addToVariantDict :: MMiSSVariantDict a -> Attributes -> a -> IO ()
addToVariantDict (MMiSSVariantDict mVar) attributes value =
   do
      (keys1 :: [Maybe String]) <- mapM
         (\ str -> getValueOpt attributes str)
         variantAttributes
      let
         (keys2 :: [(Int,Maybe String)]) =
            zip [1..] keys1
         (keys3 :: [(Int,String)]) = mapMaybe
            (\ (i,sOpt) -> fmap (\ s -> (i,s)) sOpt)
            keys2
      dict <- takeMVar mVar
      putMVar mVar (addKey dict keys3 value)


-- -------------------------------------------------------------------
-- The pure(ish) interface starts here
-- VariantDict
-- -------------------------------------------------------------------

---
-- The integer and list encode the number of the attribute to test and
--    what tests should be made.
--    Invariants: 
--       (1) the (Int,String) pairs, ordered lexicographically, increase
--           and are distinct in the VariantDict list.
--       (2) In each (Int,_,VariantDict), the integer is less than any of
--           those in the VariantDict.
data VariantDict a = VariantDict [(Int,String,VariantDict a)] (Maybe a)

emptyVariantDict :: VariantDict a
emptyVariantDict = VariantDict [] Nothing

-- -------------------------------------------------------------------
-- Matching against a value
-- -------------------------------------------------------------------

search :: VariantDict a -> (String -> IO (Maybe String)) 
   -> IO (Maybe a)
search (VariantDict branches simple) getAttribute =
   do
      let
         doRest [] = return simple
         doRest ((level,key,dict):rest) =
            do
               let
                  attKey = variantAttributesArray ! level
               valOpt <- getAttribute attKey
               let
                  matched = case valOpt of
                     Nothing -> True
                     Just str -> (str == key)
               if matched
                  then
                     do
                        searched <- search dict getAttribute
                        case searched of
                           Nothing -> doRest rest
                           _ -> return searched
                  else
                     doRest rest 
      doRest branches

-- -------------------------------------------------------------------
-- Adding a new key.
-- -------------------------------------------------------------------

---
-- Add a key.  The key data is specified by a list of (Int,String) pairs,
-- where the integers specify the relevant variant attribute and MUST BE
-- IN INCREASING ORDER.
addKey :: VariantDict a -> [(Int,String)] -> a -> VariantDict a
addKey (VariantDict list _) [] value = (VariantDict list (Just value))
addKey (VariantDict list1 def) ((level,key):restKeys) value =
   let
      -- set things up for using ExtendedPrelude.insertOrdAlternate.
      comp (level1,key1,_) (level2,key2,_) =
         compare (level1,key1) (level2,key2)
      addEmptyDict [] = (VariantDict [] (Just value))
      addEmptyDict ((level1,key1):rest) =
         (VariantDict [(level1,key1,addEmptyDict rest)] Nothing)

      ifNotFound = (level,key,addEmptyDict restKeys)

      ifFound (level2,key2,dict) = (level2,key2,addKey dict restKeys value)

      list2 = insertOrdAlternate comp ifNotFound ifFound list1
   in
      (VariantDict list2 def)
   
-- -------------------------------------------------------------------
-- Instances of HasCodedValue
-- -------------------------------------------------------------------


variantDict_tyRep = mkTyRep "MMiSSVariant" "VariantDict"
instance HasTyRep1 VariantDict where
   tyRep1 _ = variantDict_tyRep

instance HasCodedValue a => HasCodedValue (VariantDict a) where
   encodeIO = mapEncodeIO (\ (VariantDict l def) -> (l,def))
   decodeIO = mapDecodeIO (\ (l,def) -> VariantDict l def)

mmissVariantDict_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariantDict"
instance HasTyRep1 MMiSSVariantDict where
   tyRep1 _ = variantDict_tyRep

instance HasCodedValue a => HasCodedValue (MMiSSVariantDict a) where
   encodeIO (MMiSSVariantDict mVar) codedValue view =
      do
         dict <- readMVar mVar
         encodeIO dict codedValue view
   decodeIO codedValue0 view =
      do
         (dict,codedValue1) <- decodeIO codedValue0 view
         mVar <- newMVar dict
         return (MMiSSVariantDict mVar,codedValue1)
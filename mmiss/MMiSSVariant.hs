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
   variantDictSearchExact,
   addToVariantDict,
   queryInsert,

--   emptyMMiSSSearchObject,
--   MMiSSSearchObject,

   -- For the time being, newtype variants of MMiSSSearchObject.
   MMiSSVariantSearch,
   MMiSSVariantSpec,
   refineVariantSearch,
   toMMiSSVariantSearchFromXml,
   toMMiSSVariantSpecFromXml,
   fromMMiSSVariantSpecToXml,
   fromMMiSSVariantSpecToAttributes,
   toMMiSSVariantSpecFromAttributes,
   fromMMiSSSpecToSearch,

   emptyMMiSSVariantSearch,
   emptyMMiSSVariantSpec,

--   toMMiSSSearchObject,
--   fromMMiSSSearchObject,
--   toMMiSSSearchObjectFromXml,
--   mergeMMiSSSearchObjects,

--   mkVariantAttributes,
--   extractVariantAttributes,
   ) where

#include "config.h"

import Maybe
import List
import Monad

import Array
import Concurrent
import FiniteMap
import Set
import Exception

import ExtendedPrelude
import Dynamics
import Registry
import Computation(done)


#if HAXMLINT
import Text.XML.HaXml.Types(Attribute,AttValue(..))
#else
import XmlTypes(Attribute,AttValue(..))
#endif

import CodedValue
import BasicObjects
import AttributesType


import MMiSSDTDAssumptions
---
-- variantAttributes itself is imported from MMiSSDTDAssumptions.
variantAttributesArray :: Array Int String
variantAttributesArray = 
   listArray (1,length variantAttributes) variantAttributes

variantAttributesSet :: Set String
variantAttributesSet = mkSet variantAttributes

-- -------------------------------------------------------------------
-- The impure interface
-- -------------------------------------------------------------------

newtype MMiSSVariantDict a = MMiSSVariantDict (MVar (VariantDict a))

newEmptyVariantDict :: IO (MMiSSVariantDict a)
newEmptyVariantDict = 
   do
      mVar <- newMVar emptyVariantDict
      return (MMiSSVariantDict mVar)

variantDictSearch :: MMiSSVariantDict a -> MMiSSVariantSearch -> 
   IO (Maybe a)
variantDictSearch (MMiSSVariantDict mVar) (MMiSSVariantSearch searchObject) =
   do
      dict <- readMVar mVar
      return (searchVariantDict dict searchObject)

variantDictSearchExact :: MMiSSVariantDict a -> MMiSSVariantSpec -> 
   IO (Maybe a)
variantDictSearchExact (MMiSSVariantDict mVar) (MMiSSVariantSpec searchObject)
      =
   do
      dict <- readMVar mVar
      return (searchVariantDictExact dict searchObject)

addToVariantDict :: MMiSSVariantDict a -> MMiSSVariantSpec -> a -> IO ()
addToVariantDict (MMiSSVariantDict mVar) (MMiSSVariantSpec searchObject) value
      =
   do
      dict <- takeMVar mVar
      putMVar mVar (addToPureVariantDict dict searchObject value)

---
-- Query if, when were immediately to insert a value with the second
-- set of attributes (this), and then search with the first set (parent)
-- we would get the value back.
-- We use a ghastly exceptions-trick to insert a pseudo-value into the
--    dictionary.
-- The method looks inefficient since we have to create a new dictionary;
--    however I hope that lazy evaluation will prevent too much work being
--    done. 
queryInsert :: MMiSSVariantDict a -> MMiSSVariantSpec -> MMiSSVariantSearch
   -> IO Bool
queryInsert (MMiSSVariantDict mVar) 
      (MMiSSVariantSpec parent) (MMiSSVariantSearch this) =
   do
      dict <- readMVar mVar
      let 
         dict2 = addToPureVariantDict dict this (error "#")
      catchJust 
         errorCalls
         ( (fromJust (searchVariantDict dict2 parent)) `seq` (return False))
         (\ str -> if str == "#" then return True else error str)


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
-- The MMiSSSearchObject type, its relatives, and the instances of Eq
-- and Ord.
-- -------------------------------------------------------------------

---
-- Contains variant attributes
newtype MMiSSSearchObject 
   = MMiSSSearchObject (FiniteMap String String) deriving Eq

---
-- An MMiSSVariantSearch is something used to select from a collection
-- of different variants.
newtype MMiSSVariantSearch 
   = MMiSSVariantSearch MMiSSSearchObject deriving (Eq,Ord)

---
-- An MMiSSVariantSpec is something used to specify a particular variant.
newtype MMiSSVariantSpec 
   = MMiSSVariantSpec MMiSSSearchObject deriving (Eq,Ord)

---
-- We need to define an ordering on MMiSSSearchObject

instance Ord MMiSSSearchObject where
   compare = mapOrd (\ (MMiSSSearchObject fm) -> fmToList fm)

-- -------------------------------------------------------------------
-- Functions for MMiSSVariantSearch/Spec
-- -------------------------------------------------------------------

emptyMMiSSVariantSearch :: MMiSSVariantSearch
emptyMMiSSVariantSearch = MMiSSVariantSearch emptyMMiSSSearchObject

emptyMMiSSVariantSpec :: MMiSSVariantSpec
emptyMMiSSVariantSpec = MMiSSVariantSpec emptyMMiSSSearchObject

refineVariantSearch :: MMiSSVariantSearch -> MMiSSVariantSpec
    -> MMiSSVariantSearch
refineVariantSearch (MMiSSVariantSearch searchObject1) 
      (MMiSSVariantSpec searchObject2) 
   = (MMiSSVariantSearch (mergeMMiSSSearchObjects searchObject1 searchObject2))

toMMiSSVariantSearchFromXml :: [Attribute] -> MMiSSVariantSearch
toMMiSSVariantSearchFromXml attributes 
   = MMiSSVariantSearch (toMMiSSSearchObjectFromXml attributes)

toMMiSSVariantSpecFromXml :: [Attribute] -> MMiSSVariantSpec
toMMiSSVariantSpecFromXml attributes
   = MMiSSVariantSpec (toMMiSSSearchObjectFromXml attributes)

fromMMiSSVariantSpecToXml :: MMiSSVariantSpec -> [Attribute]
fromMMiSSVariantSpecToXml (MMiSSVariantSpec searchObject)
   = fromMMiSSSearchObjectToXml searchObject

fromMMiSSVariantSpecToAttributes :: MMiSSVariantSpec -> IO Attributes
fromMMiSSVariantSpecToAttributes (MMiSSVariantSpec searchObject) =
   fromMMiSSSearchObject searchObject

toMMiSSVariantSpecFromAttributes :: Attributes -> IO MMiSSVariantSpec
toMMiSSVariantSpecFromAttributes attributes =
   do
      searchObject <- toMMiSSSearchObject attributes
      return (MMiSSVariantSpec searchObject)

fromMMiSSSpecToSearch :: MMiSSVariantSpec -> MMiSSVariantSearch
fromMMiSSSpecToSearch (MMiSSVariantSpec searchObject) =
   MMiSSVariantSearch searchObject

-- -------------------------------------------------------------------
-- Searching by a map to strings, and handling inheritance of these values.
-- -------------------------------------------------------------------

---
-- No settings for variant attributes
emptyMMiSSSearchObject :: MMiSSSearchObject
emptyMMiSSSearchObject = MMiSSSearchObject emptyFM 

---
-- Get the variant attributes
toMMiSSSearchObject :: Attributes -> IO MMiSSSearchObject
toMMiSSSearchObject attributes =
  do
     (attributePairs :: [Maybe (String,String)]) <-
        mapM
           (\ attKey ->
              do
                 strOpt <- getValueOpt attributes attKey
                 case strOpt of
                    Nothing -> return Nothing
                    Just "" -> return Nothing
                    Just str -> return (Just (attKey,str))
              )
           variantAttributes
     return (MMiSSSearchObject (listToFM (catMaybes attributePairs)))

---
-- Extracting an Attributes value from an MMiSSSearchObject
fromMMiSSSearchObject :: MMiSSSearchObject -> IO Attributes
fromMMiSSSearchObject (MMiSSSearchObject fm) =
   do
      let
         (list :: [(String,String)]) = fmToList fm

         view = error "Oops - I didn't realise MMiSSVariant.hs needed a view!"

      attributes <- newEmptyAttributes view
      mapM_
         (\ (key,value) -> setValue attributes key value)
         list
      return attributes

---
-- Get the variant attributes from a set of Xml attributes
toMMiSSSearchObjectFromXml :: [Attribute] -> MMiSSSearchObject
toMMiSSSearchObjectFromXml attributes =
   MMiSSSearchObject (listToFM [ 
      (key,value) | 
         (key,AttValue [Left value]) <- attributes,
         elementOf key variantAttributesSet 
      ])

---
-- Get the Xml attributes from the variant attributes
fromMMiSSSearchObjectToXml :: MMiSSSearchObject -> [Attribute]
fromMMiSSSearchObjectToXml (MMiSSSearchObject fmap) =
   map
      (\ (key,value) -> (key,AttValue [Left value]))
      (fmToList fmap)

---
-- Search a dictionary by an MMiSSSearchObject
searchVariantDict :: VariantDict a -> MMiSSSearchObject -> Maybe a
searchVariantDict (VariantDict branches simple) 
      (searchObject @ (MMiSSSearchObject attMap)) =
   let
      doRest [] = simple
      doRest ((level,key,dict):rest) =
         do
            let
               attKey = variantAttributesArray ! level
               valOpt = lookupFM attMap attKey
            let
               matched = case valOpt of
                  Nothing -> True
                  Just str -> (str == key)
            if matched
               then
                  case searchVariantDict dict searchObject of
                     Nothing -> doRest rest
                     searched -> searched
               else
                  doRest rest
   in
      doRest branches

---
-- Search a dictionary by an MMiSSSearchObject, where we insist that
-- we only match accept an exact match.
searchVariantDictExact :: VariantDict a -> MMiSSSearchObject -> Maybe a
searchVariantDictExact variantDict searchObject =
   let
      intKeys = convertMMiSSSearchObject searchObject               
      doRest [] (VariantDict _ simple) = simple
      doRest ((index,val) : rest) (VariantDict branches _) =
         let
            findFun (index2,val2,dict2) =
               if (index2,val2) == (index,val) then Just dict2 else Nothing
         in
            case findJust findFun branches of
               Nothing -> Nothing
               Just dict -> doRest rest dict
   in
      doRest intKeys variantDict

---
-- Merge two search objects.  The settings in the second one take priority.
mergeMMiSSSearchObjects :: MMiSSSearchObject -> MMiSSSearchObject 
   -> MMiSSSearchObject
mergeMMiSSSearchObjects (MMiSSSearchObject map1) (MMiSSSearchObject map2) =
   (MMiSSSearchObject (plusFM map1 map2))

-- -------------------------------------------------------------------
-- Adding to a variant dictionary
-- -------------------------------------------------------------------


---
-- Add a key, the key data being specified by a set of String options,
-- in the order of variantAttributes.
addToPureVariantDict :: VariantDict a -> MMiSSSearchObject -> a 
   -> VariantDict a
addToPureVariantDict variantDict mmissSearchObject value =
   addKey variantDict (convertMMiSSSearchObject mmissSearchObject) value

---
-- Convert an MMiSSSearchObject into (Int,String) representation, listing
-- the set attributes by index
convertMMiSSSearchObject :: MMiSSSearchObject -> [(Int,String)]
convertMMiSSSearchObject (MMiSSSearchObject attMap) =
   let
      (keys1 :: [Maybe String]) 
         = map (\ str -> lookupFM attMap str) variantAttributes
      (keys2 :: [(Int,Maybe String)]) =
         zip [1..] keys1
      (keys3 :: [(Int,String)]) = mapMaybe
         (\ (i,sOpt) -> fmap (\ s -> (i,s)) sOpt)
         keys2
   in
      keys3
  
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

mmissVariantSpec_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariantSpec"
instance HasTyRep MMiSSVariantSpec where
   tyRep _ = mmissVariantSpec_tyRep

instance HasCodedValue MMiSSVariantSpec where
   encodeIO = mapEncodeIO (\ (MMiSSVariantSpec searchObject) -> searchObject)
   decodeIO = mapDecodeIO (\ searchObject -> MMiSSVariantSpec searchObject)

mmissSearchObject_tyRep = mkTyRep "MMiSSVariant" "MMiSSSearchObject"
instance HasTyRep MMiSSSearchObject where
   tyRep _ = mmissSearchObject_tyRep

instance HasCodedValue MMiSSSearchObject where
   encodeIO = mapEncodeIO (\ (MMiSSSearchObject map) -> fmToList map)
   decodeIO = mapDecodeIO (\ list -> MMiSSSearchObject (listToFM list))

-- -------------------------------------------------------------------
-- Other utility functions involving variant attributes
-- -------------------------------------------------------------------

---
-- Filling out unset fields.  We replace these by "" for now.
mkVariantAttributes :: Attributes -> IO ()
mkVariantAttributes atts =
   do
      let
         fillIn str =
            do
               valueOpt <- getValueOpt atts str
               case valueOpt of
                  Just ( _ :: String) -> done
                  Nothing -> setValue atts str ""
      mapM_ fillIn variantAttributes

---
-- Filter a set of Xml Attributes to extract only the variant attributes
extractVariantAttributes :: [Attribute] -> [Attribute]
extractVariantAttributes attributes =
   filter
      (\ (name,attVal) -> elementOf name variantAttributesSet
         )
      attributes
   
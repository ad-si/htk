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

   variantAttributesType2,

   -- Merging
   getMMiSSVariantDictObjectLinks, 
   attemptMergeMMiSSVariantDict,
   MMiSSVariants,

   displayMMiSSVariantDictKeys, -- :: MMiSSVariantDict object -> IO ()
      -- put up a window describing the keys in the object.
   ) where

import Maybe
import Monad

import Control.Concurrent.MVar
import Data.FiniteMap

import Computation (done)
import AtomString
import Registry
import ExtendedPrelude
import Dynamics

import LogWin

import GraphOps

import ViewType
import View
import VersionGraphClient
import BasicObjects
import CodedValue
import MergeTypes
import AttributesType

import Text.XML.HaXml.Types(Attribute,AttValue(..))

import MMiSSDTDAssumptions

-- -----------------------------------------------------------------------
-- Implementation Note
-- 
-- This is a complete reimplementation of this module.  The previous version
-- used a clever indexing strategy which was unfortunately extremely 
-- inflexible.  This version uses a more general scoring method, in which
-- we score each available variant, and then pick the highest-scoring.
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- The list of variant attributes
-- -----------------------------------------------------------------------

data VariantAttribute = VariantAttribute {
   key :: String,
   score :: Maybe String -> Maybe String -> IO Integer,
      -- first argument is the value being searched for,
      -- second the value of this attribute.
      -- Nothing in either case means "unset".

      -- Positive scores mean it matches more.
   innerIgnored :: Bool
      -- True if outer settings of this attribute are NOT overruled by an
      -- inner one (when expanding document).  
   }

-- The list of all available attributes
-- NB.  The function setVersion and getVersion are going to assume that
-- the versionVariant comes first.
variants :: [VariantAttribute]
variants =
   versionVariant :
      map
         (\ key -> VariantAttribute {
            key = key,
            innerIgnored = False,
            score = (\ s1Opt s2Opt ->
               return (case (s1Opt,s2Opt) of
                  (Just s1,Just s2) -> 
                     if s1 == s2 then 1000 else 0
                  (Nothing,Nothing) -> 500
                  _ -> 100
                  )
               )
            })
         MMiSSDTDAssumptions.variantAttributes

-- the most complicated attribute, the one which controls versions,
-- which we score highly.
versionVariant :: VariantAttribute
versionVariant = VariantAttribute {
   key = "Version",
   innerIgnored = True,
   score = (\ s1Opt s2Opt ->
      case (s1Opt,s2Opt) of
         (Just searchKey,Just thisKey) -> 
            if searchKey == thisKey
               then
                  return 8000
               else
                  do
                     let
                        searchNode = versionToNode (fromString searchKey)
                        thisNode = versionToNode (fromString thisKey)

                     is <- isAncestor versionGraph thisNode searchNode

                     return (if is then 4000 else 0)
         (Just searchKey,Nothing) -> return 0
         (Nothing,_) -> return 500
      )
   }
           
-- -----------------------------------------------------------------------
-- The other datatypes
-- -----------------------------------------------------------------------

newtype MMiSSVariantSpec = MMiSSVariantSpec MMiSSVariants deriving (Eq,Ord)

newtype MMiSSVariantSearch = MMiSSVariantSearch MMiSSVariants deriving (Eq,Ord)

newtype MMiSSVariants = MMiSSVariants [(VariantAttribute,String)]
   -- The elements of this list are in the same order as the variants list.

-- ------------------------------------------------------------------------
-- Functions and Instances for MMiSSVariants
-- ----------------------------------------------------------------------- 

---
-- Construct MMiSSVariants, given a list of pairs (key,value)
mkMMiSSVariants :: [(String,String)] -> MMiSSVariants
mkMMiSSVariants theseVariants =
   MMiSSVariants (
      mapMaybe
         (\ variant ->
            findJust
               (\ (thisKey,thisValue) -> 
                  if thisKey == key variant
                     then
                        Just (variant,thisValue)
                     else
                        Nothing
                  )
               theseVariants
            )
         variants
      )

unmkMMiSSVariants :: MMiSSVariants -> [(String,String)]
unmkMMiSSVariants (MMiSSVariants pairs) 
   = map (\ (variant,thisValue) -> (key variant,thisValue)) pairs

instance Eq MMiSSVariants where
   (==) = mapEq unmkMMiSSVariants

instance Ord MMiSSVariants where
   compare = mapOrd unmkMMiSSVariants


getAsList :: MMiSSVariants -> [(VariantAttribute,Maybe String)]
getAsList (MMiSSVariants pairs) =
   let
      gAL :: [(VariantAttribute,String)] -> [VariantAttribute] 
         -> [(VariantAttribute,Maybe String)]
      gAL (theseVAVs @ ((thisVA,thisVal):restVAVs)) (headVA:restVAs) =
         if key thisVA == key headVA 
            then 
               (headVA,Just thisVal) : (gAL restVAVs restVAs)
            else
               (headVA,Nothing) : (gAL theseVAVs restVAs)
      gAL [] (headVA : restVAs) =
               (headVA,Nothing) : (gAL [] restVAs)
      gAL [] [] = []
   in
      gAL pairs variants

-- Compare a searched-for value (first argument) with a variant.
scoreMMiSSVariants :: MMiSSVariants -> MMiSSVariants -> IO Integer
scoreMMiSSVariants variants1 variants2 =
   let
      triples :: [(VariantAttribute,Maybe String,Maybe String)]
      triples = 
            zipWith
               (\ (va,value1Opt) (_,value2Opt) -> (va,value1Opt,value2Opt))
               (getAsList variants1) (getAsList variants2)

   in
      foldM 
         (\ score0 (va,s1Opt,s2Opt) ->
            do
               thisScore <- score va s1Opt s2Opt
               return (score0 + thisScore)
            )
         0
         triples 

-- Get and set the version variant
getVersion :: MMiSSVariants -> Maybe String
getVersion (MMiSSVariants list) = case list of
   ((VariantAttribute {key = key1},value) : _)
      | key1 == key versionVariant  -> Just value
   _ -> Nothing

setVersion :: MMiSSVariants -> String -> MMiSSVariants
setVersion (MMiSSVariants list0) value = 
   let
      list1 = case list0 of
         ((VariantAttribute {key = key1},_) : rest) 
            | key1 == key versionVariant  -> rest
         _ -> list0
   in
      MMiSSVariants ((versionVariant,value) : list1)

getVersionAndUnset :: MMiSSVariants -> Maybe (String,MMiSSVariants)
getVersionAndUnset (MMiSSVariants list0) = case list0 of
   ((VariantAttribute {key = key1},value) : list1 )
      | key1 == key versionVariant  -> Just (value,MMiSSVariants list1)
   _ -> Nothing


-- ------------------------------------------------------------------------
-- More sophisticated functions for manipulating MMiSSVariantSpec and
-- MMiSSVariantSearch
-- ------------------------------------------------------------------------


refineVariantSearch :: MMiSSVariantSearch -> MMiSSVariantSpec
    -> MMiSSVariantSearch
refineVariantSearch 
      (MMiSSVariantSearch variantsOuter) 
      (MMiSSVariantSpec variantsInner) =
   let
      outerList :: [(VariantAttribute,Maybe String)]
      outerList = getAsList variantsOuter
 
      innerList :: [(VariantAttribute,Maybe String)]
      innerList = getAsList variantsInner

      refinedList1 :: [(VariantAttribute,Maybe String)]
      refinedList1 = zipWith
         (\ (va1,outerOpt) (_,innerOpt) -> 
            let
               refinedOpt = case (outerOpt,innerOpt) of
                  (Just outer,Just inner) -> 
                     Just (if innerIgnored va1
                        then
                           outer
                        else
                           inner
                        )
                  (Just outer,Nothing) -> Just outer
                  (Nothing,Just inner) -> Just inner
                  (Nothing,Nothing) -> Nothing
            in
               (va1,refinedOpt)
            ) 
         outerList innerList

      refinedList2 :: [(VariantAttribute,String)]
      refinedList2 = mapMaybe
         (\ (va,refinedOpt) -> fmap (\ refined -> (va,refined)) refinedOpt)
         refinedList1
   in
      MMiSSVariantSearch (MMiSSVariants refinedList2)

emptyVariants :: MMiSSVariants 
emptyVariants = MMiSSVariants []

emptyMMiSSVariantSearch :: MMiSSVariantSearch
emptyMMiSSVariantSearch = MMiSSVariantSearch emptyVariants

emptyMMiSSVariantSpec :: MMiSSVariantSpec
emptyMMiSSVariantSpec = MMiSSVariantSpec emptyVariants

---
-- NB.  We do not allow variant versions to be specified in XML!
toMMiSSVariantsFromXml :: [Attribute] -> MMiSSVariants
toMMiSSVariantsFromXml attributes =
   mkMMiSSVariants [ (key1,value) | 
      (key1,AttValue [Left value]) <- attributes,
      key1 /= key versionVariant 
      ]
  
  

toMMiSSVariantSearchFromXml :: [Attribute] -> MMiSSVariantSearch
toMMiSSVariantSearchFromXml attributes 
   = MMiSSVariantSearch (toMMiSSVariantsFromXml attributes)

toMMiSSVariantSpecFromXml :: [Attribute] -> MMiSSVariantSpec
toMMiSSVariantSpecFromXml attributes
   = MMiSSVariantSpec (toMMiSSVariantsFromXml attributes)


-- Get the Xml attributes from the variant attributes
fromMMiSSVariantsToXml :: MMiSSVariants -> [Attribute]
fromMMiSSVariantsToXml variants =
   map
      (\ (key,value) -> (key,AttValue [Left value]))
      (unmkMMiSSVariants variants)

 
fromMMiSSVariantSpecToXml :: MMiSSVariantSpec -> [Attribute]
fromMMiSSVariantSpecToXml (MMiSSVariantSpec variants)
   = fromMMiSSVariantsToXml variants

fromMMiSSVariantSpecToAttributes :: MMiSSVariantSpec -> IO Attributes
fromMMiSSVariantSpecToAttributes (MMiSSVariantSpec variants) =
   fromMMiSSVariants variants

toMMiSSVariantSpecFromAttributes :: Attributes -> IO MMiSSVariantSpec
toMMiSSVariantSpecFromAttributes attributes =
   do
      variants <- toMMiSSVariants attributes
      return (MMiSSVariantSpec variants)

-- | Type of variant attributes including also the version attribute
variantAttributesType2 :: AttributesType
variantAttributesType2 
   = needs (mkAttributeKey (key versionVariant)) "" variantAttributesType 

fromMMiSSSpecToSearch :: MMiSSVariantSpec -> MMiSSVariantSearch
fromMMiSSSpecToSearch (MMiSSVariantSpec variants) =
   MMiSSVariantSearch variants


-- ------------------------------------------------------------------------
-- Interface via the Attributes type.
-- ------------------------------------------------------------------------

toMMiSSVariants :: Attributes -> IO MMiSSVariants
toMMiSSVariants attributes =
  do
     (attributeKeys :: [String]) <- listKeys attributes

     (attributePairs :: [Maybe (String,String)]) <-
        mapM
           (\ key ->
              do
                 strOpt <- getValueOpt attributes key
                 case strOpt of
                    Nothing -> return Nothing
                    Just "" -> return Nothing
                    Just str -> return (Just (key,str))
              )
           attributeKeys
     return (mkMMiSSVariants (catMaybes attributePairs))

---
-- Extracting an Attributes value from an MMiSSSearchObject
fromMMiSSVariants :: MMiSSVariants -> IO Attributes
fromMMiSSVariants mmissVariants =
   do
      let
         (list :: [(String,String)]) = unmkMMiSSVariants mmissVariants

         view = error "Oops - I didn't realise MMiSSVariant.hs needed a view!"

      attributes <- newEmptyAttributes view
      mapM_
         (\ (key,value) -> setValue attributes key value)
         list
      return attributes

-- ------------------------------------------------------------------------
-- The dictionary and functions for it.
-- ------------------------------------------------------------------------

newtype MMiSSVariantDict a = MMiSSVariantDict (Registry MMiSSVariants a)


newEmptyVariantDict :: IO (MMiSSVariantDict a)
newEmptyVariantDict =
   do
      registry <- newRegistry
      return (MMiSSVariantDict registry)

variantDictSearch :: MMiSSVariantDict a -> MMiSSVariantSearch -> IO (Maybe a)
variantDictSearch variantDict search =
   do
      resultOpt <- variantDictSearchGeneral variantDict search
      return (fmap (\ (_,_,a) -> a) resultOpt)


-- Like variantDictSearch, but also returns an IO action which removes
-- any Version attribute on the dictionary element.
-- (unused)
variantDictSearchWithDirty :: MMiSSVariantDict a -> MMiSSVariantSearch ->
   IO (Maybe (a,IO ()))
variantDictSearchWithDirty (variantDict @ (MMiSSVariantDict registry)) search =
   do
      resultOpt <- variantDictSearchGeneral variantDict search
      return (fmap 
         (\ (_,variants,a) -> 
            let
               dirtyAct =
                  case getVersionAndUnset variants of
                     Nothing -> done
                     Just (_,dirtyVariants) ->
                        changeKey registry variants dirtyVariants
            in
               (a,dirtyAct)
            )
        resultOpt
        )   

variantDictSearchGeneral :: MMiSSVariantDict a -> MMiSSVariantSearch 
      -> IO (Maybe (Integer,MMiSSVariants,a))
variantDictSearchGeneral 
      ((MMiSSVariantDict registry) :: MMiSSVariantDict a) 
      (MMiSSVariantSearch searchVariants) =
   do
      (contents :: [(MMiSSVariants,a)]) <- listRegistryContents registry

      (scored :: [(Integer,MMiSSVariants,a)]) <- mapM
         (\ (thisVariants,a) -> 
            do
               thisScore <- scoreMMiSSVariants searchVariants thisVariants
               return (thisScore,thisVariants,a)
            ) 
         contents

      case scored of
         [] -> return Nothing
         _ -> 
            let
               mr = foldr1
                  (\ (mr1 @ (max1,_,_)) (mr2 @ (max2,_,_)) ->
                     if max1 >= max2 then mr1 else mr2
                     )
                  scored
            in
               return (Just mr)


variantDictSearchExact :: MMiSSVariantDict a -> MMiSSVariantSpec -> 
   IO (Maybe a)
variantDictSearchExact (MMiSSVariantDict registry) (MMiSSVariantSpec variants)
   = getValueOpt registry variants


addToVariantDict :: MMiSSVariantDict a -> MMiSSVariantSpec -> a -> IO ()
addToVariantDict (MMiSSVariantDict registry) (MMiSSVariantSpec variants) a 
  = 
    setValue registry variants a

delFromVariantDict :: MMiSSVariantDict a -> MMiSSVariantSpec -> IO ()
delFromVariantDict (MMiSSVariantDict registry) (MMiSSVariantSpec variants) =
   deleteFromRegistry registry variants

queryInsert :: MMiSSVariantDict a -> MMiSSVariantSpec -> MMiSSVariantSearch
   -> IO Bool
queryInsert variantDict 
      (variantSpec @ (MMiSSVariantSpec thisVariants)) 
      (variantSearch @ (MMiSSVariantSearch searchVariants)) =
   do
      searchOpt <- variantDictSearchGeneral variantDict variantSearch
      case searchOpt of
         Nothing -> return True
         Just (score1,foundVariants,_) ->
            do
               score2 <- scoreMMiSSVariants searchVariants thisVariants
               return (case compare score1 score2 of
                  LT -> True
                  GT -> False
                  EQ -> 
                     -- uh-oh.  We guess that the scoring returns
                     -- earlier variants first
                     thisVariants <= foundVariants
                  )

-- ------------------------------------------------------------------------
-- Instances Typeable and CodedValue for MMiSSVariantSpec/Search
-- ------------------------------------------------------------------------

mmissVariantSpec_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariantSpec"
instance HasTyRep MMiSSVariantSpec where
   tyRep _ = mmissVariantSpec_tyRep

mmissVariantSearch_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariantSearch"
instance HasTyRep MMiSSVariantSearch where
   tyRep _ = mmissVariantSearch_tyRep

mmissVariants_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariants"
instance HasTyRep MMiSSVariants where
   tyRep _ = mmissVariants_tyRep

instance HasCodedValue MMiSSVariants where
   encodeIO = mapEncodeIO unmkMMiSSVariants
   decodeIO = mapDecodeIO mkMMiSSVariants

instance HasCodedValue MMiSSVariantSpec where
   encodeIO = mapEncodeIO 
      (\ (MMiSSVariantSpec variants) -> unmkMMiSSVariants variants)
   decodeIO = mapDecodeIO
      (\ list -> MMiSSVariantSpec (mkMMiSSVariants list))

instance HasCodedValue MMiSSVariantSearch where
   encodeIO = mapEncodeIO 
      (\ (MMiSSVariantSearch variants) -> unmkMMiSSVariants variants)
   decodeIO = mapDecodeIO
      (\ list -> MMiSSVariantSearch (mkMMiSSVariants list))

-- ------------------------------------------------------------------------
-- Instances of Typeable and CodedValue for MMiSSVariantDict.
-- ------------------------------------------------------------------------

mmissVariantDict_tyRep = mkTyRep "MMiSSVariant" "MMiSSVariantDict"
instance HasTyRep1 MMiSSVariantDict where
   tyRep1 _ = mmissVariantDict_tyRep

-- The CodedValue instance also has the job of reassigning the variant values
-- in the dictionary which are previously Nothing to the version of the
-- new view.
instance HasCodedValue a => HasCodedValue (MMiSSVariantDict a) where
   decodeIO = mapDecodeIO (\ registry -> MMiSSVariantDict registry)

   encodeIO (mmissVariantDict @ (MMiSSVariantDict registry)) codedValue0 view =
      do
         versionOpt <- readMVar (committingVersion view)
         case versionOpt of
            Nothing -> done
            Just version -> setUnsetVersions mmissVariantDict version
         codedValue1 <- encodeIO registry codedValue0 view
         return codedValue1
 
-- This function does the reassignation.
setUnsetVersions :: MMiSSVariantDict a -> Version -> IO ()
setUnsetVersions ((MMiSSVariantDict registry) :: MMiSSVariantDict a) 
      newVersion =
   do
      let
         versionString = toString newVersion
      (registryContents :: [(MMiSSVariants,a)])
         <- listRegistryContents registry 
      let
         toReAssign :: [(MMiSSVariants,a)]
         toReAssign = 
            filter
               (\ (variant,_) -> not (isJust (getVersion variant)))
               registryContents

      mapM_
         (\ (oldVariant,value) -> 
            do
               deleteFromRegistry registry oldVariant
               setValue registry (setVersion oldVariant versionString) value
            )
         toReAssign


-- ------------------------------------------------------------------------
-- Merging variant dictionaries
-- ------------------------------------------------------------------------

getMMiSSVariantDictObjectLinks 
   :: (a -> IO (ObjectLinks key)) -> MMiSSVariantDict a 
   -> IO ((ObjectLinks (MMiSSVariants,key)))
getMMiSSVariantDictObjectLinks 
      (getIndividualObjectLinks :: a -> IO (ObjectLinks key)) 
      ((MMiSSVariantDict registry) :: MMiSSVariantDict a) =
   do
      (contents :: [(MMiSSVariants,a)]) <- listRegistryContents registry
      (result :: [ObjectLinks (MMiSSVariants,key)]) <- mapM
         (\ (variants,a) ->
            do
               objectLinks <- getIndividualObjectLinks a
               return (fmap (\ key -> (variants,key)) objectLinks)
            )
         contents
      return (concatObjectLinks result)

attemptMergeMMiSSVariantDict
   :: (View -> a -> IO a) -> [(View,MMiSSVariantDict a)] 
   -> IO (MMiSSVariantDict a)
attemptMergeMMiSSVariantDict converter 
      (variantDicts :: [(View,MMiSSVariantDict a)]) =
   -- We assume, because of the versions, that there are no clashes.
   -- Thus our only job is to get each list and converted.
   do
      (converted :: [[(MMiSSVariants,a)]]) <- mapM
         (\ (view,MMiSSVariantDict registry) ->
            do
               (list1 :: [(MMiSSVariants,a)]) <- listRegistryContents registry
               list2 <- mapM
                  (\ (variant,a0) ->
                     do
                        a1 <- converter view a0
                        return (variant,a1)
                     )
                  list1
               return list2
            )
         variantDicts

      registry <- listToNewRegistry (concat converted)
      return (MMiSSVariantDict registry)

-- ------------------------------------------------------------------------
-- Converting things to String's.
-- ------------------------------------------------------------------------

instance Show MMiSSVariants where
   show mmissVariants = 
      let
         contents :: [(String,String)]
         contents = unmkMMiSSVariants mmissVariants

         showItem :: (String,String) -> String
         showItem (key,value) = key ++ "=" ++ value

         showItems :: [String] -> String
         showItems [] = "Variant with no attributes"
         showItems items = unsplitByChar ',' items
      in
         showItems (map showItem contents)

instance Show MMiSSVariantSpec where
   show (MMiSSVariantSpec variants) = show variants

displayMMiSSVariantDictKeys :: MMiSSVariantDict object -> IO ()
displayMMiSSVariantDictKeys (MMiSSVariantDict registry) =
   do
      (keys :: [MMiSSVariants]) <- listKeys registry
      let
         description = case keys of
            [] -> "Object contains no variants"
            _ -> unsplitByChar '\n' (map show keys)
      win <- createLogWin []
      writeLogWin win description


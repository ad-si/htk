-- | This module controls selection by variant attributes.
-- 
-- The problem we solve is search a database MMiSSVariantDict a
-- of a-values indexed by partial maps from variantAttributes strings
-- (listed in the variable of the same name) to Strings.
-- 
-- When searching by a key (such a map), an item in the database
--    indexed by another key (such a map) matches if the two maps
--    agree wherever they are both defined.  Thus the null-value
--    corresponds to a wildcard.
-- Where more than one item in the database is defined, we choose
--    some item in the database for which the domain of its map
--    is maximal.
module MMiSSVariant(
   MMiSSVariantDict,
   newEmptyVariantDict,
   variantDictSearch,
   variantDictSearchWithSpec,
   variantDictSearchExact,
   variantDictSearchAlmostExact,
   variantDictSearchAlmostExactWithSpec,

   addToVariantDict,
   queryInsert,

   -- For the time being, newtype variants of MMiSSSearchObject.
   MMiSSVariantSearch,
   MMiSSVariantSpec,
   refineVariantSearch,
   addToVariantSearch,
   removeFromVariantSearch,
   toMMiSSVariantSearchFromXml,
   toMMiSSVariantSpecFromXml,
   fromMMiSSVariantSpecToXml,
   fromMMiSSVariantSpecToAttributes,
   toMMiSSVariantSpecFromAttributes,
   fromMMiSSSpecToSearch,

   fromMMiSSVariantSpec,
   toMMiSSVariantSpec,

   emptyMMiSSVariantSearch,
   emptyMMiSSVariantSpec,

   variantAttributesType2,
   variantAttributes2, -- :: [String]
   -- List of all variant attributes including also the version attribute

   -- Merging
   getMMiSSVariantDictObjectLinks, 
   attemptMergeMMiSSVariantDict,
   variantDictsSame,
   MMiSSVariants,

   displayMMiSSVariantDictKeys, -- :: MMiSSVariantDict object -> IO ()
      -- describing the keys in the object.

   copyVariantDict, 
      -- :: (ObjectVersion -> IO VersionInfo) -> MMiSSVariantDict object 
      -- -> IO (MMiSSVariantDict object)
      -- This operation is used to update the version numbers when copying
      -- a VariantDict object from one repository to another.

   HasGetAllVariants(..),
      -- Is possible to get all variants of this dictionary.  MMiSSVariantDict
      -- instances it.

   mergeMMiSSVariantSpecStrict,
      -- :: MMiSSVariantSpec -> MMiSSVariantSpec
      -- -> WithError MMiSSVariantSpec
      -- Merge two variantSpecs, or complain when the same key is given
      -- two identical values.
   ) where

import Maybe
import Monad

import Control.Concurrent.MVar

import Computation(done,fromWithError,WithError,hasValue,hasError,mapWithError)
import AtomString
import Registry
import ExtendedPrelude
import Dynamics
import Messages

import LogWin

import VersionInfo

import ViewType
import View
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
 
      let
         -- Awful temporary hack so we prefer higher version numbers.
         toNum :: Maybe String -> Maybe Integer
         toNum (Just s) = readCheck s
         toNum Nothing = Nothing
      in
         return (
            case toNum s1Opt of
               Nothing -> 
                  -- we prefer higher version numbers, but Nothing  better.
                  case toNum s2Opt of
                     Just i -> 4096*i
                     Nothing -> 10000000000000 
                        -- hopefully bigger than any version number
               Just searchNumber ->
                  -- we prefer version numbers <= this.
                  case toNum s2Opt of
                     Nothing -> -1000
                     Just thisNumber -> 
                        if thisNumber <= searchNumber
                           then
                              4096*thisNumber
                           else
                              -4096*searchNumber
            )
      )
   }
           
-- -----------------------------------------------------------------------
-- The other datatypes
-- -----------------------------------------------------------------------

newtype MMiSSVariantSpec = MMiSSVariantSpec MMiSSVariants 
   deriving (Eq,Ord,Typeable)

newtype MMiSSVariantSearch = MMiSSVariantSearch MMiSSVariants 
   deriving (Eq,Ord,Typeable)

newtype MMiSSVariants = MMiSSVariants [(VariantAttribute,String)]
   deriving (Typeable)
   -- The elements of this list are in the same order as the variants list.

-- ------------------------------------------------------------------------
-- Functions and Instances for MMiSSVariants
-- ----------------------------------------------------------------------- 

-- | Construct MMiSSVariants, given a list of pairs (key,value)
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
-- Converting MMiSSVariantSpec to and from lists of Strings.
-- ------------------------------------------------------------------------

fromMMiSSVariantSpec :: MMiSSVariantSpec -> [(String,String)]
fromMMiSSVariantSpec (MMiSSVariantSpec variants) =
   unmkMMiSSVariants variants
   
toMMiSSVariantSpec :: [(String,String)] -> WithError MMiSSVariantSpec
toMMiSSVariantSpec strs =
   do
      checkValid strs
      return (MMiSSVariantSpec (mkMMiSSVariants strs))

checkValid :: [(String,String)] -> WithError ()
checkValid strs =
   do
      mapM_
         (\ (key0,_) ->
            if any (\ va -> key va == key0) variants
               then
                  done
               else
                  fail ("Unknown variant attribute " ++ key0)
            )
         strs
      case findDuplicate fst strs of
         Just (key0,_) -> fail ("Key " ++ key0 ++ " occurs more than once")
         Nothing -> done
     
      

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

-- | NB.  We do not allow variant versions to be specified in XML!
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


-- | List of all variant attributes including also the version attribute
variantAttributes2 :: [String]
variantAttributes2 = (key versionVariant) : variantAttributes

-- | Type of variant attributes including also the version attribute
variantAttributesType2 :: AttributesType
variantAttributesType2 
   = needs (mkAttributeKey (key versionVariant)) "" variantAttributesType 

fromMMiSSSpecToSearch :: MMiSSVariantSpec -> MMiSSVariantSearch
fromMMiSSSpecToSearch (MMiSSVariantSpec variants) =
   MMiSSVariantSearch variants

-- | (addToVariantSearch variantSearch key value) sets key=value for 
-- variantSearch.
addToVariantSearch :: MMiSSVariantSearch -> String -> String 
   -> WithError MMiSSVariantSearch
addToVariantSearch variantSearch key0 value0 =
   let
      variantSpec = MMiSSVariantSpec (mkMMiSSVariants [(key0,value0)])
   in
      if all (\ att -> key att /= key0) variants
         then
            hasError ("No variant attribute " ++ key0 ++ " known")
         else
            hasValue (refineVariantSearch variantSearch variantSpec)

-- | (removeFromVariantSearch variantSearch key) removes the setting of
-- key from variantSearch (or complains if there is none)
removeFromVariantSearch :: MMiSSVariantSearch -> String 
   -> WithError MMiSSVariantSearch
removeFromVariantSearch (MMiSSVariantSearch variants0) key0 =
   mapWithError MMiSSVariantSearch (removeFromVariants variants0 key0)

-- MMiSSVariants function for unsetting a value
removeFromVariants :: MMiSSVariants -> String  -> WithError MMiSSVariants
removeFromVariants (MMiSSVariants list0) key0 =
   case deleteAndFindFirstOpt (\ (va,_) -> key va == key0) list0 of
      Nothing -> hasError ("Key " ++ key0 ++ " is unset")
      Just (_,list1) -> hasValue (MMiSSVariants list1)

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

-- | Extracting an Attributes value from an MMiSSSearchObject
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
   deriving (Typeable)


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

variantDictSearchWithSpec :: MMiSSVariantDict a -> MMiSSVariantSearch ->
   IO (Maybe (a,MMiSSVariantSpec))
variantDictSearchWithSpec variantDict search =
   do
      resultOpt <- variantDictSearchGeneral variantDict search
      return 
         (fmap (\ (_,variants,a) -> (a,MMiSSVariantSpec variants)) resultOpt)

{-
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
-}

variantDictSearchGeneral :: MMiSSVariantDict a -> MMiSSVariantSearch 
      -> IO (Maybe (Integer,MMiSSVariants,a))
variantDictSearchGeneral dict (MMiSSVariantSearch variants) =
   variantDictSearchVeryGeneral (const True) dict variants

variantDictSearchVeryGeneral 
   :: (MMiSSVariants -> Bool) -> MMiSSVariantDict a -> MMiSSVariants 
   -> IO (Maybe (Integer,MMiSSVariants,a))
variantDictSearchVeryGeneral 
      filterFn ((MMiSSVariantDict registry) :: MMiSSVariantDict a) 
      searchVariants =
   do
      (contents0 :: [(MMiSSVariants,a)]) <- listRegistryContents registry

      let
         contents1 = filter
            (\ (variants,_) -> filterFn variants)
            contents0

      (scored :: [(Integer,MMiSSVariants,a)]) <- mapM
         (\ (thisVariants,a) -> 
            do
               thisScore <- scoreMMiSSVariants searchVariants thisVariants
               return (thisScore,thisVariants,a)
            ) 
         contents1

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


-- | variantDictSearchExact looks for a variant with exactly the same spec.
variantDictSearchExact :: MMiSSVariantDict a -> MMiSSVariantSpec -> 
   IO (Maybe a)
variantDictSearchExact (MMiSSVariantDict registry) (MMiSSVariantSpec variants)
   = getValueOpt registry variants

-- | variantDictSearchAlmostExact looks for the most closely matching variant
-- whose spec is exactly the same as the argument, EXCEPT for the
-- version.
variantDictSearchAlmostExact :: MMiSSVariantDict a -> MMiSSVariantSpec ->
   IO (Maybe a)
variantDictSearchAlmostExact variantDict variantSpec =
   do
      resultOpt <- variantDictSearchAlmostExactWithSpec variantDict variantSpec
      return (fmap
         fst
         resultOpt
         )

variantDictSearchAlmostExactWithSpec 
   :: MMiSSVariantDict a -> MMiSSVariantSpec -> IO (Maybe (a,MMiSSVariantSpec))
variantDictSearchAlmostExactWithSpec variantDict (MMiSSVariantSpec variants) =
   do
      let
         removeVersion :: MMiSSVariants -> MMiSSVariants
         removeVersion variants0 =
            fromMaybe variants0 (fmap snd (getVersionAndUnset variants0))

         variantsWithoutVersion = removeVersion variants

         filterFn variants0 = removeVersion variants0 == variantsWithoutVersion

      resultOpt <- variantDictSearchVeryGeneral filterFn variantDict 
            variantsWithoutVersion
         -- not a very efficient way of doing, but better to have as
         -- few search functions as possible.
      return (fmap (\ (_,variants,a) -> (a,MMiSSVariantSpec variants))
         resultOpt)

addToVariantDict :: MMiSSVariantDict a -> MMiSSVariantSpec -> a -> IO ()
addToVariantDict (MMiSSVariantDict registry) (MMiSSVariantSpec variants) a 
  = 
    setValue registry variants a

{-
delFromVariantDict :: MMiSSVariantDict a -> MMiSSVariantSpec -> IO ()
delFromVariantDict (MMiSSVariantDict registry) (MMiSSVariantSpec variants) =
   deleteFromRegistry registry variants
-}

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

instance Monad m => HasBinary MMiSSVariants m where
   writeBin = mapWrite unmkMMiSSVariants
   readBin = mapRead mkMMiSSVariants

instance Monad m => HasBinary MMiSSVariantSpec m where
   writeBin = mapWrite 
      (\ (MMiSSVariantSpec variants) -> unmkMMiSSVariants variants)
   readBin = mapRead
      (\ list -> MMiSSVariantSpec (mkMMiSSVariants list))

instance Monad m => HasBinary MMiSSVariantSearch m where
   writeBin = mapWrite
      (\ (MMiSSVariantSearch variants) -> unmkMMiSSVariants variants)
   readBin = mapRead
      (\ list -> MMiSSVariantSearch (mkMMiSSVariants list))

-- ------------------------------------------------------------------------
-- Instances of CodedValue for MMiSSVariantDict.
-- ------------------------------------------------------------------------

-- The CodedValue instance also has the job of reassigning the variant values
-- in the dictionary which are previously Nothing to the version of the
-- new view.
instance HasBinary a CodingMonad 
      => HasBinary (MMiSSVariantDict a) CodingMonad where

   readBin = mapRead (\ registry -> MMiSSVariantDict registry)

   writeBin = mapWriteViewIO (\ view 
         (mmissVariantDict @ (MMiSSVariantDict registry)) ->
      do
         versionOpt <- readMVar (committingVersion view)
         case versionOpt of
            Nothing -> done
            Just version -> setUnsetVersions mmissVariantDict version
         return registry
      )
 
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

variantDictsSame :: (a -> a -> Bool) 
   -> MMiSSVariantDict a -> MMiSSVariantDict a -> IO Bool
variantDictsSame testEq (MMiSSVariantDict registry1) 
      (MMiSSVariantDict registry2) =
   do
      (list1 :: [(MMiSSVariants,a)]) <- listRegistryContents registry1
      (list2 :: [(MMiSSVariants,a)]) <- listRegistryContents registry2

      return (
         and (zipWith 
               (\ (var1,a1) (var2,a2) -> var1 == var2 && testEq a1 a2)
               list1 list2
               )
         &&  
            (length list1 == length list2)
         )

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

instance Show MMiSSVariantSearch where
   show (MMiSSVariantSearch variants) = show variants

descriptionMMiSSVariantDictKeys :: MMiSSVariantDict object -> IO String
descriptionMMiSSVariantDictKeys (MMiSSVariantDict registry) =
   do
      (keys :: [MMiSSVariants]) <- listKeys registry
      let
         description = case keys of
            [] -> "Object contains no variants"
            _ -> unsplitByChar '\n' (map show keys)
      return description

displayMMiSSVariantDictKeys :: MMiSSVariantDict object -> IO ()
displayMMiSSVariantDictKeys variantDict =
   do
      description <- descriptionMMiSSVariantDictKeys variantDict
      htkPres <- htkPresent
      if htkPres
         then
            do
               win <- createLogWin []
               writeLogWin win description
         else
            messageMess description

-- ------------------------------------------------------------------------
-- Copying a VariantDict.
-- ------------------------------------------------------------------------

-- | This operation is used to update the version numbers when copying
-- a VariantDict object from one repository to another.
copyVariantDict 
   :: (ObjectVersion -> IO VersionInfo) -> MMiSSVariantDict object 
   -> IO (MMiSSVariantDict object)
copyVariantDict getNewVersionInfo (MMiSSVariantDict registry0) =
   do
      contents0 <- listRegistryContents registry0
      contents1 <- mapM
         (\ (variants0,object) ->
            do
               variants1 <- copyVariants getNewVersionInfo variants0
               return (variants1,object)
            )
         contents0
      registry1 <- listToNewRegistry contents1
      return (MMiSSVariantDict registry1)
      

copyVariants
   :: (ObjectVersion -> IO VersionInfo) -> MMiSSVariants
   -> IO MMiSSVariants
copyVariants getNewVersionInfo variants0 =
   let
      versionStringOpt = getVersion variants0
   in
      case versionStringOpt of
         Nothing -> return variants0
         Just versionString0 -> 
            case fromWithError (fromStringWE versionString0) of
               Left _ ->
                  do
                     putStrLn ("Mysterious version number " ++ versionString0
                        ++ "; why?")
                     return variants0
               Right (version0 :: ObjectVersion) ->
                  do
                     versionInfo1 <- getNewVersionInfo version0
                     let
                        version1 = version (user versionInfo1)

                        variants1 = setVersion variants0 (toString version1)
                     return variants1

-- -----------------------------------------------------------------------
-- Extracting ALL the objects of an MMiSSVariantDict
-- -----------------------------------------------------------------------

class HasGetAllVariants dict object | dict -> object where
   getAllVariants :: dict -> IO [(MMiSSVariantSpec,object)]

instance HasGetAllVariants (MMiSSVariantDict object) object where
   getAllVariants (MMiSSVariantDict registry :: MMiSSVariantDict object) =
      do
         (contents0 :: [(MMiSSVariants,object)]) 
            <- listRegistryContents registry
         let
            contents1 =
               map
                  (\ (vars,object) -> (MMiSSVariantSpec vars,object))
                  contents0 
         return contents1

-- -----------------------------------------------------------------------
-- Merging MMiSSVariant(Specs) where we insist no attribute is specified
-- twice.
-- -----------------------------------------------------------------------

mergeMMiSSVariantsStrict :: MMiSSVariants -> MMiSSVariants 
   -> WithError MMiSSVariants 
mergeMMiSSVariantsStrict variants1 variants2 =
   do
      (combinedList1 :: [(VariantAttribute,Maybe String)]) <-
         zipWithM
            (\ (va1,val1Opt) (_,val2Opt) -> case (val1Opt,val2Opt) of
               (Just val1,Just val2) | val1 /= val2
                  -> fail ("Multiply-specified variant " ++ key va1 
                     ++ " has incompatible values " ++ val1 ++ " and "
                     ++ val2)
               (Just val1,_) -> return (va1,val1Opt)
               _ -> return (va1,val2Opt)
               )
            (getAsList variants1)
            (getAsList variants2)
        
      let
         combinedList2 :: [(VariantAttribute,String)]
         combinedList2 = mapMaybe
            (\ (va,valOpt) -> fmap (\ val -> (va,val)) valOpt)
            combinedList1
      return (MMiSSVariants combinedList2)

mergeMMiSSVariantSpecStrict :: MMiSSVariantSpec -> MMiSSVariantSpec
   -> WithError MMiSSVariantSpec
mergeMMiSSVariantSpecStrict (MMiSSVariantSpec variants1) 
      (MMiSSVariantSpec variants2) =
   do
      variants <- mergeMMiSSVariantsStrict variants1 variants2
      return (MMiSSVariantSpec variants)

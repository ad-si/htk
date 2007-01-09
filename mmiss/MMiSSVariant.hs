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
   fromMMiSSSpecToSearch,

   fromMMiSSVariantSpec,
   toMMiSSVariantSpec,

   emptyMMiSSVariantSearch,
   emptyMMiSSVariantSpec,

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


   editMMiSSVariantSearch,
      -- :: MMiSSVariantSearch -> IO (Maybe MMiSSVariantSearch)
   editMMiSSVariantSpec,
      -- :: MMiSSVariantSpec -> IO (Maybe MMiSSVariantSpec)
      -- Let the user edit an MMiSSVariantSearch or Spec.
   ) where

import Maybe
import Monad

import Control.Concurrent.MVar
import DeprecatedFiniteMap
import Text.XML.HaXml.Types hiding (VersionInfo)

import Computation(done,fromWithError,WithError,mapWithError)
import AtomString
import Registry
import ExtendedPrelude
import Dynamics
import Messages

import LogWin

import VersionInfo

import ViewType
import View
import CodedValue
import MergeTypes

import MMiSSGetVariantAttributes

-- -----------------------------------------------------------------------
-- Implementation Note
-- 
-- This is a complete reimplementation of this module.  The previous version
-- used a clever indexing strategy which was unfortunately extremely 
-- inflexible.  This version uses a more general scoring method, in which
-- we score each available variant, and then pick the highest-scoring.
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- The basic type for variants
-- -----------------------------------------------------------------------

-- | The basic type for variants
data MMiSSVariants = MMiSSVariants {
   versionOpt :: Maybe ObjectVersion,
   keyValues :: [(String,Maybe String)]
      -- ^ key-value pairs.  The keys must be in strictly increasing order,
      -- and none of them should be "Version".
   } deriving (Eq,Ord,Typeable)


-- -----------------------------------------------------------------------
-- Accessing the version
-- -----------------------------------------------------------------------

-- Get and set the version variant
getVersion :: MMiSSVariants -> Maybe ObjectVersion
getVersion = versionOpt

setVersion :: MMiSSVariants -> ObjectVersion -> MMiSSVariants
setVersion variants version0 = variants {versionOpt = Just version0} 

getVersionAndUnset :: MMiSSVariants -> Maybe (ObjectVersion,MMiSSVariants)
getVersionAndUnset variants =
   do
      version1 <- versionOpt variants
      return (version1,variants {versionOpt = Nothing})


-- -----------------------------------------------------------------------
-- Turning MMiSSVariants into a set of key-value pairs and back again.
-- Here "Version" is used for the ObjectVersion.
-- -----------------------------------------------------------------------

mkMMiSSVariants :: [(String,Maybe String)] -> MMiSSVariants
mkMMiSSVariants keyValues =
   let
      variantsMap1 :: FiniteMap String (Maybe String)
      variantsMap1 = listToFM keyValues

      versionOpt :: Maybe ObjectVersion
      versionOpt =
         do
            versionStrOpt <- lookupFM variantsMap1 versionKey
            versionStr <- case versionStrOpt of
               Nothing -> error ("Nothing given as Version attribute")
               Just versionStr -> return versionStr

            case fromWithError (fromStringWE versionStr) of
               Left _ -> error ("Incomprehensible Version attribute "
                  ++ show versionStr)
               Right version -> return version

      variantsMap2 :: FiniteMap String (Maybe String)
      variantsMap2 =
         if isJust versionOpt
            then
               delFromFM variantsMap1 versionKey
            else
               variantsMap1
   in
      
      MMiSSVariants {versionOpt = versionOpt,keyValues = fmToList variantsMap2}

unmkMMiSSVariants :: MMiSSVariants -> [(String,Maybe String)]
unmkMMiSSVariants variants =
   let
      l1 :: [(String,Maybe String)]
      l1 = keyValues variants
   in
      case versionOpt variants of
         Nothing -> l1
         Just version1 -> (versionKey,Just (toString version1)) : l1


-- | Convert a variant-attributes element as described in the DTD
-- to an MMiSSVariants.
-- NB.  This assumes the element is a valid <variant-attributes> element.
-- Otherwise expect match failures and the like.
toMMiSSVariantsFromXml :: Element -> MMiSSVariants 
toMMiSSVariantsFromXml (Elem "variant-attributes" [] content) =
   let
      doOne :: Content -> (String,Maybe String)
      doOne (CElem (Elem "attribute" attributes [])) =
         let
            Just keyAtt = lookup "key" attributes
            valueAttOpt = lookup "value" attributes
           
            getAtt (AttValue [Left value]) = value
 
            key = getAtt keyAtt
            valueOpt = fmap getAtt valueAttOpt
        in
            (key,valueOpt)

   in
      mkMMiSSVariants (map doOne content)

-- | Reverse of toMMiSSVariantsFromXml.
fromMMiSSVariantsToXml :: MMiSSVariants -> Element
fromMMiSSVariantsToXml variants =
   let
      variantsAsStrings :: [(String,Maybe String)]
      variantsAsStrings = unmkMMiSSVariants variants

      doOne :: (String,Maybe String) -> Element
      doOne (key,valueOpt) =
         let
            valueAttributes = case valueOpt of
               Nothing -> []
               Just value -> [("value",AttValue [Left value])]
         in
            Elem "attribute" (("key",AttValue [Left key]):valueAttributes) []
   in
      Elem "variant-attributes" [] (map (CElem . doOne) variantsAsStrings)



-- -----------------------------------------------------------------------
-- Working out how close two variants are to each other.
-- -----------------------------------------------------------------------

-- | Compare two variants.  A high score means it matches more.
--
-- NB.  The scoring method used is very crude; it is hoped that one day
-- Achim will be able to improve it.
scoreVariants 
   :: MMiSSVariants -- ^ Variants being searched for
   -> MMiSSVariants -- ^ Some variants
   -> Integer
scoreVariants variantsS variantsT =
   let
      versionScore :: Integer
      -- This is weighted heavily, since if the user specifies a particular
      -- version it is probable that he actually wants it.
      versionScore = case (versionOpt variantsS,versionOpt variantsT) of
         (Nothing,_) -> 0
         (Just version,Nothing) -> -10 
            -- I don't think this should happen really anyway.
         (Just versionS,Just versionT) ->
            if versionS >= versionT
               then 
                  -- versions <= the one we are looking for are good, since
                  -- at least a version checked out with the version we
                  -- are looking for should contain them. 
                  10*(
                     max 
                        (1000 - 
                           (getVersionInteger versionS 
                              - getVersionInteger versionT))
                        1
                     )
               else
                  0 -- future versions are not so good.


      noMatchPenalty = -10
      doesNotExistPenalty = -5
      matchBonus = 3

      keyScore :: 
         [(String,Maybe String)] -> [(String,Maybe String)] -> Integer
      keyScore [] _ = 0
      keyScore l [] = doesNotExistPenalty*(fromIntegral (length l))
      keyScore (variantS @ ((keyS,valueS):restS)) 
            (variantT @ ((keyT,valueT):restT))=
         case compare keyS keyT of
            EQ  -> 
               let
                  scoreThis = 
                     if valueS == valueT then matchBonus else noMatchPenalty
               in
                  scoreThis + keyScore restS restT
            LT -> doesNotExistPenalty + keyScore restS variantT
            GT -> keyScore variantS restT
   in
      versionScore + keyScore (keyValues variantsS) (keyValues variantsT)

-- -----------------------------------------------------------------------
-- Refining an MMiSSVariants with an inner MMiSSVariants
-- -----------------------------------------------------------------------

-- | In the resulting variants, the key or values in the first MMiSS variants
-- are added to those in the second MMiSSVariants, where they are not
-- already supplied.  However the version is always taken from the second
-- argument.
refineMMiSSVariants :: MMiSSVariants -> MMiSSVariants -> MMiSSVariants
refineMMiSSVariants variantsO variantsI =
   let
      refineKVs :: [(String,Maybe String)] -> [(String,Maybe String)] 
         -> [(String,Maybe String)] 
      refineKVs [] l = l
      refineKVs l [] = l
      refineKVs
            (kvsO@((kvO@(keyO,_)):restO))
            (kvsI@((kvI@(keyI,_)):restI))
         = case compare keyO keyI of
            EQ -> kvI : (refineKVs restO restI)
            LT -> kvO : (refineKVs restO kvsI)
            GT -> kvI : (refineKVs kvsO restI)
   in
      MMiSSVariants {
         versionOpt = versionOpt variantsI,
         keyValues = refineKVs (keyValues variantsO) (keyValues variantsI)
         }


--  | Combining two MMiSSVariants where we insist there is no conflict.
mergeMMiSSVariantsStrict :: MMiSSVariants -> MMiSSVariants 
   -> WithError MMiSSVariants 
mergeMMiSSVariantsStrict variants1 variants2 =
   do
      versionOpt <- case (versionOpt variants1,versionOpt variants2) of
         (Nothing,Nothing) -> return Nothing
         (jA,Nothing) -> return jA
         (Nothing,jB) -> return jB
         (jC@(Just v1),Just v2) -> 
            if v1 == v2
               then
                  return jC
               else
                  fail ("Conflicting versions " ++ show v1 
                     ++ " and " ++ show v2) 
      let
         combineKeys :: [(String,Maybe String)] -> [(String,Maybe String)]
            -> WithError [(String,Maybe String)]
         combineKeys [] l2 = return l2
         combineKeys l1 [] = return l1
         combineKeys (l1@((kv1@(k1,v1)):rest1)) (l2@((kv2@(k2,v2)):rest2)) =
            case compare k1 k2 of
               LT -> 
                  do
                     rest <- combineKeys rest1 l2
                     return (kv1:rest)
               GT ->
                  do
                     rest <- combineKeys l1 rest2
                     return (kv2:rest)
               EQ -> 
                  if v1 == v2 
                     then
                        do
                           rest <- combineKeys rest1 rest2
                           return (kv1:rest)
                     else
                        fail ("Multiply-specified variant " ++ k1 
                           ++ " has incompatible values " ++ show v1
                           ++ " and " ++ show v2)

      keyValues <- combineKeys (keyValues variants1) (keyValues variants2)
      return (MMiSSVariants {versionOpt = versionOpt,keyValues = keyValues})


-- -----------------------------------------------------------------------
-- Other functions for MMiSSVariants.
-- -----------------------------------------------------------------------

emptyVariants :: MMiSSVariants 
emptyVariants = MMiSSVariants {versionOpt = Nothing,keyValues = []}


-- MMiSSVariants function for unsetting a value
removeFromVariants :: MMiSSVariants -> String  -> WithError MMiSSVariants
removeFromVariants variants key0 =
   do
      let
         list0 = keyValues variants
      list1 <-
         case deleteAndFindFirstOpt (\ (key1,_) -> key1 == key0) list0 of
            Nothing -> fail ("Key " ++ key0 ++ " is unset")
            Just (_,list1) -> return list1
      return (variants {keyValues = list1})

-- -----------------------------------------------------------------------
-- The other datatypes
-- -----------------------------------------------------------------------

newtype MMiSSVariantSpec = MMiSSVariantSpec MMiSSVariants 
   deriving (Eq,Ord,Typeable)

newtype MMiSSVariantSearch = MMiSSVariantSearch MMiSSVariants 
   deriving (Eq,Ord,Typeable)

-- ------------------------------------------------------------------------
-- Converting MMiSSVariantSpec to and from lists of Strings.
-- ------------------------------------------------------------------------

fromMMiSSVariantSpec :: MMiSSVariantSpec -> [(String,Maybe String)]
fromMMiSSVariantSpec (MMiSSVariantSpec variants) =
   unmkMMiSSVariants variants
   
toMMiSSVariantSpec :: [(String,Maybe String)] -> WithError MMiSSVariantSpec
toMMiSSVariantSpec strs =
   do
      checkValid strs
      return (MMiSSVariantSpec (mkMMiSSVariants strs))

checkValid :: [(String,Maybe String)] -> WithError ()
checkValid strs =
   do
      mapM_
         (\ (key0,_) ->
            if key0 /= versionKey
               then
                  done
               else
                  fail ("Illegitimate Version in toMMiSSVariantSpec")
            )
         strs
      case findDuplicate fst strs of
         Just (key0,_) -> fail ("Key " ++ key0 ++ 
            " occurs more than once in toMMiSSVariantSpec")
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
   (MMiSSVariantSearch (refineMMiSSVariants variantsOuter variantsInner))

emptyMMiSSVariantSearch :: MMiSSVariantSearch
emptyMMiSSVariantSearch = MMiSSVariantSearch emptyVariants

emptyMMiSSVariantSpec :: MMiSSVariantSpec
emptyMMiSSVariantSpec = MMiSSVariantSpec emptyVariants
  
toMMiSSVariantSearchFromXml :: Element -> MMiSSVariantSearch
toMMiSSVariantSearchFromXml element
   = MMiSSVariantSearch (toMMiSSVariantsFromXml element)

toMMiSSVariantSpecFromXml :: Element -> MMiSSVariantSpec
toMMiSSVariantSpecFromXml element
   = MMiSSVariantSpec (toMMiSSVariantsFromXml element)

fromMMiSSVariantSpecToXml :: MMiSSVariantSpec -> Element
fromMMiSSVariantSpecToXml (MMiSSVariantSpec variants) =
   fromMMiSSVariantsToXml variants

fromMMiSSSpecToSearch :: MMiSSVariantSpec -> MMiSSVariantSearch
fromMMiSSSpecToSearch (MMiSSVariantSpec variants) =
   MMiSSVariantSearch variants

-- | (addToVariantSearch variantSearch key value) sets key=value for 
-- variantSearch.
addToVariantSearch :: MMiSSVariantSearch -> String -> String 
   -> WithError MMiSSVariantSearch
addToVariantSearch variantSearch key0 value0 =
   let
      variantSpec = MMiSSVariantSpec (mkMMiSSVariants [(key0,Just value0)])
   in
      if key0 == versionKey
         then
            fail "addToVariantSearch used with Version key"
         else
            return (refineVariantSearch variantSearch variantSpec)

-- | (removeFromVariantSearch variantSearch key) removes the setting of
-- key from variantSearch (or complains if there is none)
removeFromVariantSearch :: MMiSSVariantSearch -> String 
   -> WithError MMiSSVariantSearch
removeFromVariantSearch (MMiSSVariantSearch variants0) key0 =
   mapWithError MMiSSVariantSearch (removeFromVariants variants0 key0)

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

      
         (scored :: [(Integer,MMiSSVariants,a)]) = map
            (\ (thisVariants,a) -> 
               let
                  thisScore = scoreVariants searchVariants thisVariants
               in
                  (thisScore,thisVariants,a)
               ) 
            contents1

      return (
         case scored of
            [] -> Nothing
            _ -> 
               let
                  mr = foldr1
                     (\ (mr1 @ (max1,_,_)) (mr2 @ (max2,_,_)) ->
                        if max1 >= max2 then mr1 else mr2
                        )
                     scored
               in
                  Just mr
         )


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


queryInsert :: MMiSSVariantDict a -> MMiSSVariantSpec -> MMiSSVariantSearch
   -> IO Bool
queryInsert variantDict 
      (variantSpec @ (MMiSSVariantSpec thisVariants)) 
      (variantSearch @ (MMiSSVariantSearch searchVariants)) =
   do
      searchOpt <- variantDictSearchGeneral variantDict variantSearch
      return (case searchOpt of
         Nothing -> True
         Just (score1,foundVariants,_) ->
            let
               score2 = scoreVariants searchVariants thisVariants
            in
               case compare score1 score2 of
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
-- Instance of CodedValue for MMiSSVariantDict.
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
setUnsetVersions :: MMiSSVariantDict a -> ObjectVersion -> IO ()
setUnsetVersions ((MMiSSVariantDict registry) :: MMiSSVariantDict a) 
      newVersion =
   do
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
               setValue registry (setVersion oldVariant newVersion) value
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
         contents :: [(String,Maybe String)]
         contents = unmkMMiSSVariants mmissVariants

         showItem :: (String,Maybe String) -> String
         showItem (key,Just value) = key ++ "=" ++ value
         showItem (key,Nothing) = key

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
      versionOpt = getVersion variants0
   in
      case versionOpt of
         Nothing -> return variants0
         Just (version0 :: ObjectVersion) ->
            do
               versionInfo1 <- getNewVersionInfo version0
               let
                  version1 = version (user versionInfo1)

                  variants1 = setVersion variants0 version1
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


mergeMMiSSVariantSpecStrict :: MMiSSVariantSpec -> MMiSSVariantSpec
   -> WithError MMiSSVariantSpec
mergeMMiSSVariantSpecStrict (MMiSSVariantSpec variants1) 
      (MMiSSVariantSpec variants2) =
   do
      variants <- mergeMMiSSVariantsStrict variants1 variants2
      return (MMiSSVariantSpec variants)

-- -----------------------------------------------------------------------
-- Editing variant attributes
-- -----------------------------------------------------------------------

editMMiSSVariantSpec :: MMiSSVariantSpec -> IO (Maybe MMiSSVariantSpec)
editMMiSSVariantSpec (MMiSSVariantSpec oldVariants) =
   do
      newVariantsOpt <- editMMiSSVariants oldVariants
      return (fmap MMiSSVariantSpec newVariantsOpt)


editMMiSSVariantSearch :: MMiSSVariantSearch -> IO (Maybe MMiSSVariantSearch)
editMMiSSVariantSearch (MMiSSVariantSearch oldVariants) =
   do
      newVariantsOpt <- editMMiSSVariants oldVariants
      return (fmap MMiSSVariantSearch newVariantsOpt)

editMMiSSVariants :: MMiSSVariants -> IO (Maybe MMiSSVariants)
editMMiSSVariants oldVariants =
   do
      newVariantsListOpt 
         <- editVariantAttributes (unmkMMiSSVariants oldVariants)
      return (fmap mkMMiSSVariants newVariantsListOpt)

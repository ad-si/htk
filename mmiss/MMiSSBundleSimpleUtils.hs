-- | This module contains simple utilities for MMiSSBundles, that is to say,
-- those that we can compile without having compiled any particular repository
-- types like MMiSSObjectType or MMiSSFileType. 
module MMiSSBundleSimpleUtils(
   mergeBundles, -- :: [Bundle] -> WithError Bundle

   nameFileLoc, -- :: FileLoc -> WithError EntityName
   nameFileLocOpt, -- :: FileLoc -> WithError (Maybe EntityName) 
   describeFileLoc, -- :: FileLoc -> String

   alwaysNameFileLoc, -- :: FileLoc -> EntityName
      -- assumes no errors on parsing the EntityName, and
      -- if no name is supplied uses preambleEntityName.
   preambleEntityName, -- :: EntityName
      -- How we call preamble names.  (This will not be typeable by the user.)

   mkUnknownBundleText, -- :: CharType -> String -> BundleText

   mkBundleText, 
      -- :: (Eq object,Typeable object) 
      -- => object -> BundleText

   fromBundleTextWE,
      -- :: Typeable object => BundleText -> WithError object
   fromBundleTextIO,
      -- :: Typeable object => BundleText -> IO object

   toVariants,
      -- :: BundleNode -> [(Maybe MMiSSVariantSpec,BundleText)]

   mkOneMMiSSPackage,
      -- :: BundleNode -> BundleNode

   wrapContainingMMiSSPackage,
      -- :: Maybe EntityName -> EntityFullName -> BundleType -> BundleNodeData 
      -- -> WithError BundleNode

   bundleTextToString,
      -- :: BundleText -> String

   bundleTextToAsciiICSL,
      -- :: BundleText -> ICStringLen
      -- this also checks that CharType = Byte.

   getAllNodes, -- :: Bundle -> [(LocInfo,BundleNode)]
   getAllNodes1, -- :: Bundle -> [(LocInfo,BundleNode)]
      -- getAllNodes guarantees depth-first-search order; getAllNodes1 
      -- guarantees reverse depth-first-search order.

   bundleFoldM,
      --  :: Monad m 
      -- => (ancestorInfo -> BundleNode -> m ancestorInfo)
      -- -> (PackageId -> m ancestorInfo)
      -- -> Bundle
      -- -> m () 
      -- Scan all the nodes in a bundle, depth-first.  The ancestorInfo is 
      -- threaded down. 

   bundleNodeFoldM,bundleNodeFoldM1,
      -- :: Monad m 
      -- => (ancestorInfo -> BundleNode -> m ancestorInfo)
      -- -> ancestorInfo
      -- -> BundleNode
      --  -> m () 
      -- Scan all the nodes in a BundleNode, depth-first.  The ancestorInfo is 
      -- threaded down. 
      -- The difference between the two is that bundleFoldM1 does not
      -- visit the very top node.

   lookupNode,
      -- :: Bundle -> PackageId -> EntityFullName -> Maybe BundleNode
      -- look up a node within a bundle.

   getTag, 
      -- :: BundleNode -> WithError String
      -- get the tag of an Element.

   LocInfo(..),
   initialLocInfo, -- :: PackageId -> LocInfo
   subDir, -- :: Maybe EntityName -> Bool -> LocInfo -> WithError LocInfo
   setVariants, -- :: MMiSSVariantSpec -> LocInfo -> WithError LocInfo
   toElementInfo, -- :: LocInfo -> ElementInfo
   ) where

import Data.FiniteMap

import Monad
import Maybe

import Control.Monad.State
import Text.XML.HaXml.Types

import Computation
import ExtendedPrelude
import AtomString
import Dynamics
import UTF8
import ICStringLen

import EntityNames

import MMiSSBundle
import MMiSSImportExportErrors
import MMiSSBundleTypes
import MMiSSVariant
import MMiSSElementInfo
import MMiSSElementInstances

-- -------------------------------------------------------------------------
-- Merging bundles
-- -------------------------------------------------------------------------

mergeBundles :: [Bundle] -> WithError Bundle
mergeBundles bundles =
   do
      let
         keyedBundleNodes1 :: [(PackageId,BundleNode)]
         keyedBundleNodes1 = concat
            (map
                (\ (Bundle keyedBundleNodes0) -> keyedBundleNodes0)
                bundles
                )
      keyedBundleNodes2 <- mergeKeyedBundleNodes (==) packageIdStr
         [] keyedBundleNodes1
      return (Bundle keyedBundleNodes2)
  
mergeKeyedBundleNodes :: (key -> key -> Bool) -> (key -> String) -> [String] 
   -> [(key,BundleNode)] -> WithError [(key,BundleNode)]
   -- The first argument is an equality testing predicate, but has to
   -- be passed explicitly to avoid different mutually recursive contexts.
mergeKeyedBundleNodes eqFn (describeFn :: key -> String) backtrace inputList =
   let
      fmap1 :: FiniteMap String [(key,BundleNode)]
      fmap1 =
         foldl
            (\ map0 (kn @ (key,node)) -> 
               let
                  keyStr = describeFn key
                  entry =
                     case lookupFM map0 keyStr of
                        Nothing -> [kn]
                        Just kns -> kn:kns
               in
                  addToFM map0 keyStr entry
               )
            emptyFM
            inputList

      checkKeyEq :: [(key,BundleNode)] -> WithError key
      checkKeyEq ((k1,_):kns) =
         if all (\ (k,_) -> k `eqFn` k1) kns
            then
               return k1
            else
               mergeFailure (describeFn k1 : backtrace) 
                  "two elements with same location but different type"

      mergeOne :: [(key,BundleNode)] -> WithError (key,BundleNode)
      mergeOne bundleNodes =
         do
            key <- checkKeyEq bundleNodes
            bundleNode <- mergeBundleNodesAsOne (describeFn key:backtrace) 
               (map snd bundleNodes)
            return (key,bundleNode)
 
      (keyedBundleNodes :: [[(key,BundleNode)]]) = eltsFM fmap1
   in
      mapM
         mergeOne
         keyedBundleNodes
         


mergeBundleNodesAsOne :: [String] -> [BundleNode] -> WithError BundleNode
mergeBundleNodesAsOne backtrace [bundleNode] = return bundleNode
mergeBundleNodesAsOne backtrace bundleNodes =
   do
      let
         -- Check for clashes in FileLocs and construct a new one.  We insist
         -- on BundleTypes all being identical, but allow some names to be
         -- Nothing.

         fileLocs @ (fileLoc0 : _) = map fileLoc bundleNodes

         (nameOpts :: [Maybe String]) = map name fileLocs
         (names :: [String]) = catMaybes nameOpts
         nameOpt1 = case names of
            [] -> Nothing
            name0 : _ -> Just name0

         objectType1 = objectType fileLoc0

         fileLoc1 = FileLoc {
            name = nameOpt1,
            objectType = objectType1
            }

         isCompatible :: FileLoc -> Bool
         isCompatible (FileLoc {name = nameOpt2,objectType = objectType2}) =
            (case (nameOpt1,nameOpt2) of
               (Just name,Just name2)
                  | name /= name2 -> False
               _ -> True
            ) && (objectType1 == objectType2)
 
      if all isCompatible fileLocs
         then
            done
         else
            mergeFailure backtrace 
               "two elements with same location but different type"

      let
         bundleNodeDatas = map bundleNodeData bundleNodes

      (allObjectContents 
            :: [([BundleNode],[(Maybe MMiSSVariantSpec,BundleText)])]) <- mapM
         (\ bundleNodeData -> case bundleNodeData of
            Dir dir -> return (dir,[])
            NoData -> return ([],[])
            Object variants -> return ([],variants)
            )
         bundleNodeDatas

      let
         bundleNodes = concat (map fst allObjectContents)
         bundleVariants = concat (map snd allObjectContents)
      bundleNodeData1 <- case (bundleNodes,bundleVariants) of
         ([],[]) -> return NoData
         (bundleNodes,[]) ->
            do
               let
                  keyedNodes0 :: [(FileLoc,BundleNode)]
                  keyedNodes0 = 
                     map
                        (\ bundleNode -> (fileLoc bundleNode,bundleNode))
                        bundleNodes

                  strFileLoc2 :: FileLoc -> String
                  strFileLoc2 fileLoc = fromMaybe "(Unknown)" 
                     (strFileLoc fileLoc)
          
               (keyedNodes1 :: [(FileLoc,BundleNode)])
                  <- mergeKeyedBundleNodes (==) strFileLoc2 backtrace 
                     keyedNodes0
               return (Dir (map snd keyedNodes1))
         ([],bundleVariants) ->
            do
               (variantMap :: FiniteMap (Maybe MMiSSVariantSpec) BundleText)
                  <- foldM
                     (\ map0 (variantSpecOpt,text) ->
                        case lookupFM map0 variantSpecOpt of
                           Nothing -> return (
                              addToFM map0 variantSpecOpt text)
                           Just text0 ->
                              if text0 == text
                                 then 
                                    return map0
                                 else
                                    mergeFailure backtrace
                                       "Object has two incompatible variants"
                        )
                     emptyFM
                     bundleVariants                  
               return (Object (fmToList variantMap))
         _ ->
            mergeFailure backtrace 
               "Object has both text and directory data"    

      let
         bundleNode = BundleNode {
            fileLoc = fileLoc1,
            bundleNodeData = bundleNodeData1
            }

      return bundleNode

mergeFailure :: [String] -> String -> WithError a
mergeFailure backtrace mess =
   fail ("Error constructing bundle at " 
      ++ unsplitByChar '/' (reverse backtrace)
      ++ ": " ++ mess)

-- --------------------------------------------------------------------------
-- FileLoc functions
-- --------------------------------------------------------------------------

strFileLoc :: FileLoc -> Maybe String
strFileLoc fileLoc =
   case name fileLoc of
      Nothing -> Nothing
      Just name0 -> case ext (objectType fileLoc) of
         Nothing -> Just name0
         Just ext0 -> Just (name0 ++ [specialChar] ++ ext0)

alwaysNameFileLoc :: FileLoc -> EntityName
alwaysNameFileLoc fileLoc =
   let
      nameOpt = coerceWithError (nameFileLocOpt fileLoc)
   in
      fromMaybe preambleEntityName nameOpt

nameFileLoc :: FileLoc -> WithError EntityName
nameFileLoc fileLoc = 
   do
      nameOpt <- nameFileLocOpt fileLoc
      case nameOpt of
         Just name -> return name
         Nothing -> fail "Attempt to write file where no name is specified"

nameFileLocOpt :: FileLoc -> WithError (Maybe EntityName)
nameFileLocOpt fileLoc =
   let
      strOpt = strFileLoc fileLoc
   in
      case strOpt of
         Nothing -> hasValue Nothing
         Just str -> 
            do
               name <- fromStringWE str
               return (Just name)

describeFileLoc :: FileLoc -> String
describeFileLoc fileLoc =
   fromMaybe
      (fallBack fileLoc)
      (strFileLoc fileLoc)
   where
      fallBack :: FileLoc -> String
      fallBack fileLoc =
         "Unnamed object with type " ++ describeBundleType (objectType fileLoc)
          
describeBundleType :: BundleType -> String
describeBundleType bundleType = 
   describeBundleTypeEnum (base bundleType)
      ++ d "ext" (ext bundleType) ++ d "extra" (extra bundleType)
   where
      d :: String -> Maybe String -> String
      d key Nothing = ""
      d key (Just v) = " with " ++ key ++ "=" ++ v

describeBundleTypeEnum :: BundleTypeEnum -> String
describeBundleTypeEnum bte = case bte of
   FolderEnum -> "simple folder"
   FileEnum -> "simple file"
   MMiSSFolderEnum -> "MMiSS package folder"
   MMiSSObjectEnum -> "MMiSS object"
   MMiSSFileEnum -> "MMiSS file"
   MMiSSPreambleEnum -> "MMiSS preamble"
   UnknownType -> "Object of unknown type"

preambleEntityName :: EntityName
preambleEntityName = EntityName "#PREAMBLE"
   --  Not used for LinkedObjects but in fileLocs and so on.

-- -------------------------------------------------------------------------
-- Getting at, constructing and deconstructing BundleText's
-- -------------------------------------------------------------------------

toVariants :: BundleNode -> [(Maybe MMiSSVariantSpec,BundleText)]
toVariants node = case bundleNodeData node of
   Object variants -> variants
   _ -> []



mkUnknownBundleText :: CharType -> String -> BundleText
mkUnknownBundleText charType str =
   BundleString {
      contents = fromString str,
      charType = charType
      }

mkBundleText :: (Eq object,Typeable object) => object -> BundleText
mkBundleText object = 
   let
      dyn1 = toDyn object
      eqFn1 dyn = case fromDyn dyn of
         Nothing -> False
         Just object2 -> object == object2
   in
      BundleDyn {dyn = dyn1,eqFn = eqFn1}


fromBundleTextIO :: Typeable object => BundleText -> IO object
fromBundleTextIO bundleText 
   = coerceImportExportIO (fromBundleTextWE bundleText)

fromBundleTextWE :: Typeable object => BundleText -> WithError object
fromBundleTextWE bundleText =
   case bundleText of
      BundleDyn {dyn = dyn} -> fromDynWE dyn
      NoText -> fail 
         "Attempt to import object with no specified text"
      BundleString {} -> fail
         "MMiSS bug: unexpected BundleString in fromBundleTextIO"

bundleTextToString :: BundleText -> String
bundleTextToString NoText = importExportError "Missing text"
bundleTextToString BundleDyn {} = importExportError 
   "MMiSS bug: unexpected BundleDyn in bundleTextToString"
bundleTextToString (BundleString {contents = icsl,charType = charType}) =
   case charType of
      Byte -> toString icsl
      Unicode ->
         let
            str1 = toString icsl
         
            str2 = fromUTF8WE str1
         in
            coerceImportExport str2


bundleTextToAsciiICSL :: BundleText -> ICStringLen
bundleTextToAsciiICSL NoText = importExportError "Missing text"
bundleTextToAsciiICSL BundleDyn {} = importExportError 
   "MMiSS bug: unexpected BundleDyn in bundleTextToAsciiICSL"
bundleTextToAsciiICSL (BundleString {contents = icsl,charType = Unicode}) =
   importExportError "Unexpected Unicode in bundle text"
bundleTextToAsciiICSL (BundleString {contents = icsl,charType = Byte}) =
   icsl


-- -------------------------------------------------------------------------
-- Abbreviations for making BundleNodes and so on.
-- -------------------------------------------------------------------------

-- Make an MMiSSPackageFolder with one element
mkOneMMiSSPackage :: BundleNode -> BundleNode
mkOneMMiSSPackage node1 =
   let 
      fileLoc1 = FileLoc {
         name = Nothing,
         objectType = mmissPackageType
         }
   in
      BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = Dir [node1]
         }

-- Given the package path for an object, and the object's type and data 
-- construct the BundleNode for the package with the object
-- The first argument will be the name of the package, if supplied.
wrapContainingMMiSSPackage 
   :: Maybe EntityName -> EntityFullName -> BundleType -> BundleNodeData 
   -> WithError BundleNode
wrapContainingMMiSSPackage _ (EntityFullName []) _ _ = fail "Null packagePath!"
wrapContainingMMiSSPackage packageNameOpt fullName bundleType bundleNodeData0 =
   let
      wrapSubFolder :: EntityFullName -> BundleNode
      wrapSubFolder (EntityFullName [name0]) =
         -- construct BundleNode for the object
         let
            fileLoc0 = FileLoc {
               name = Just (toString name0),
               objectType = bundleType
               }
            bundleNode0 = BundleNode {
               fileLoc = fileLoc0,
               bundleNodeData = bundleNodeData0
               }
         in
            bundleNode0
      wrapSubFolder (EntityFullName (name0:names)) =
         -- recurse on names, and then construct an MMiSSSubFolder containing
         -- just (names)
         let
            bundleNode0 = wrapSubFolder (EntityFullName names)
            fileLoc1 = FileLoc {
               name = Just (toString name0),
               objectType = mmissSubFolderType
               }
            bundleNode1 = BundleNode {
               fileLoc = fileLoc1,
               bundleNodeData = Dir [bundleNode0]
               }
         in
            bundleNode1

      bundleNode0 = wrapSubFolder fullName
      fileLoc1 = FileLoc {
         name = fmap toString packageNameOpt,
         objectType = mmissPackageType
         }
      bundleNodeData1 = Dir [bundleNode0]
      bundleNode1 = BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         }
   in
      return bundleNode1

-- -------------------------------------------------------------------------
-- Eq for BundleText
-- -------------------------------------------------------------------------

instance Eq BundleText where
   (==) text1 text2 =
      case (text1,text2) of
         (      BundleString {contents = icsl1,charType = ct1},
                BundleString {contents = icsl2,charType = ct2}) -> 
             (icsl1 == icsl2 && ct1 == ct2)
         (BundleDyn {eqFn = eqFn1},BundleDyn {dyn = dyn2}) ->
             eqFn1 dyn2
         (NoText,NoText) -> True
         _ -> False

-- -------------------------------------------------------------------------
-- Scanning bundles
-- -------------------------------------------------------------------------

-- | Scan all the nodes in a bundle, depth-first.  The ancestorInfo is 
-- threaded down. 
bundleFoldM :: Monad m 
   => (ancestorInfo -> BundleNode -> m ancestorInfo)
   -> (PackageId -> m ancestorInfo)
   -> Bundle
   -> m () 
bundleFoldM visitNode getAncestorInfo (Bundle packageBundleNodes) =
   mapM_
      (\ (packageId,bundleNode) ->
         do
            ancestorInfo <- getAncestorInfo packageId
            bundleNodeFoldM visitNode ancestorInfo bundleNode
         )
      packageBundleNodes
      
-- | Scan all the nodes in a BundleNode, depth-first.  The ancestorInfo is 
-- threaded down. 
bundleNodeFoldM :: Monad m 
   => (ancestorInfo -> BundleNode -> m ancestorInfo)
   -> ancestorInfo
   -> BundleNode
   -> m () 
bundleNodeFoldM visitNode ancestorInfo0 bundleNode =
   do
      ancestorInfo1 <- visitNode ancestorInfo0 bundleNode
      bundleNodeFoldM1 visitNode ancestorInfo1 bundleNode

-- | Variant of bundleNodeFoldM, where we do not visit the very
-- top node. 
bundleNodeFoldM1 :: Monad m 
   => (ancestorInfo -> BundleNode -> m ancestorInfo)
   -> ancestorInfo
   -> BundleNode
   -> m () 
bundleNodeFoldM1 visitNode ancestorInfo0 bundleNode =
   do
      let
         subNodes = case bundleNodeData bundleNode of
            Dir subNodes -> subNodes
            _ -> []
      mapM_ (bundleNodeFoldM visitNode ancestorInfo0) subNodes


-- | An example application
-- returns all nodes in depth-first-search order
getAllNodes :: Bundle -> ResultList
getAllNodes bundle = reverse (getAllNodes1 bundle)

type ResultList = [(LocInfo,BundleNode)]

getAllNodes1 :: Bundle -> ResultList
getAllNodes1 bundle = execState (bundleFoldM foldFn toPackageId bundle) [] 
   where
      foldFn :: LocInfo -> BundleNode -> ResultMonad LocInfo
      foldFn locInfo0 bundleNode =
         do
            -- append name.  We special-case preambles and unparsable name.
            let
               name1 = case fromWithError (
                     nameFileLocOpt . fileLoc $ bundleNode) of
                  Left _ -> EntityName "#BADNAME"
                  Right Nothing -> preambleEntityName
                  Right (Just name1) -> name1

               locInfo1 = coerceWithError (subDir0 (Just name1) locInfo0)
            list0 <- get
            put ((locInfo1,bundleNode) : list0)
            let
               locInfo2 = case base . objectType . fileLoc $ bundleNode of
                  MMiSSFolderEnum -> insidePackageFolder0 True locInfo1
                  _ -> locInfo1
            return locInfo2

      toPackageId :: PackageId -> ResultMonad LocInfo
      toPackageId packageId = return (initialLocInfo packageId)

type ResultMonad = State ResultList

-- -----------------------------------------------------------------------
-- Looking things up in bundles
-- -----------------------------------------------------------------------

lookupNode :: Bundle -> PackageId -> EntityFullName -> Maybe BundleNode
lookupNode (Bundle packageBundles) packageId0 entityFullName =
   do
      bundleNode <- findJust
         (\ (packageId1,bundleNode) -> 
            if packageId1 == packageId0
               then
                  Just bundleNode
               else
                  Nothing
            )
         packageBundles

      lookupNodeInNode bundleNode entityFullName

lookupNodeInNode :: BundleNode -> EntityFullName -> Maybe BundleNode
lookupNodeInNode bundleNode0 (EntityFullName names0) = case names0 of
   [] -> return bundleNode0
   (name0 : names1) ->
      do
         let
            subNodes = case bundleNodeData bundleNode0 of
               Dir subNodes -> subNodes
               _ -> []

         bundleNode1 <- findJust
            (\ bundleNode1 -> 
               if (strFileLoc . fileLoc $ bundleNode1) == Just (toString name0)
                  then
                     Just bundleNode1
                  else
                     Nothing
               ) 
            subNodes

         lookupNodeInNode bundleNode1 (EntityFullName names1)

-- -----------------------------------------------------------------------
-- Extracting the tags within the elements in a BundleNode containing
-- just elements or detecting a conflict.
-- -----------------------------------------------------------------------

getTag :: BundleNode -> WithError String
   -- Possible errors:
   -- (1) conflicting tags
   -- (2) object has no variants
   -- (3) no element found.
getTag bundleNode1 =
   do
      let
         variants = case bundleNodeData bundleNode1 of
            Object variants -> variants
            _ -> []
      tags <- mapM
         (\ (_,bundleText) ->
            do
               (Elem tag _ _) <- fromBundleTextWE bundleText
               return tag
            )
         variants
      let
         err mess = fail (describeFileLoc (fileLoc bundleNode1) ++ ": " 
            ++ mess)

      case tags of
         [] -> err "No versions found of object"
         tag0:tags ->
            if all (== tag0) tags
               then
                  return tag0
               else
                  err "Conflicting tags found in elements in object"
                 
-- -----------------------------------------------------------------------
-- LocInfo contains the information which is threaded down giving the
-- position of objects within the bundle.
-- The main differences between LocInfo and MMiSSElementInfo are
-- that (1) LocInfo doesn't have a label; (2) fields are not Maybe,
-- everything has a packageId, packagePath and label; (3) the package path is 
-- stored in reverse order. 
-- -----------------------------------------------------------------------

data LocInfo = LocInfo {
   packageId :: PackageId,
   packagePath :: [EntityName], 
      -- lowest directories first.
      -- NB.  The packagePath does not include the very top directory name,
      -- if any.
   packageNameOpt1 :: Maybe EntityName,
      -- The name of the containing package, if known.
   isInitial :: Bool,
      -- True only for initialLocInfo.
   variants0 :: MMiSSVariantSpec,
   insidePackageFolder :: Bool 
      -- used to work out how to fill in blank extra fields in folders. 
   }

initialLocInfo :: PackageId -> LocInfo
initialLocInfo packageId = LocInfo {
   packageId = packageId,
   packagePath = [],
   packageNameOpt1 = Nothing,
   isInitial = True,
   variants0 = emptyMMiSSVariantSpec,
   insidePackageFolder = False
   }

-- The Bool is True if we are going inside a package folder.
subDir :: Maybe EntityName -> Bool -> LocInfo -> WithError LocInfo
subDir nameOpt isPackageFolder locInfo0 =
   do
      locInfo1 <- subDir0 nameOpt locInfo0
      return (insidePackageFolder0 isPackageFolder locInfo1)

insidePackageFolder0 :: Bool -> LocInfo -> LocInfo
insidePackageFolder0 isPackageFolder locInfo0
   = locInfo0 {
      insidePackageFolder = isPackageFolder || (insidePackageFolder locInfo0)
      }

-- This will return an error only if the name is not supplied and
-- this is not the isInitial package.
subDir0 :: Maybe EntityName -> LocInfo -> WithError LocInfo
subDir0 nameOpt locInfo0 =
   if isInitial locInfo0
      then
         return (locInfo0 {isInitial = False,packageNameOpt1 = nameOpt})
      else
         case nameOpt of
            Nothing -> fail "Missing name, where name required"
            Just name -> return (
               locInfo0 {packagePath = name : packagePath locInfo0})

setVariants :: MMiSSVariantSpec -> LocInfo -> WithError LocInfo
setVariants variantSpec locInfo0 =
   do
      variants1 <- mergeMMiSSVariantSpecStrict variantSpec (variants0 locInfo0)
      return (locInfo0 {variants0 = variants1})

toElementInfo :: LocInfo -> ElementInfo
toElementInfo locInfo =
   ElementInfo {
      packageIdOpt = Just (packageId locInfo),
      packagePathOpt1 = 
         Just . EntityFullName . reverse  . packagePath $ locInfo,
      packageNameOpt = packageNameOpt1 locInfo,
      labelOpt = Nothing,
      variants = variants0 locInfo
      }
{- This module contains the functions for converting to and from the 
   bundles described in mmiss/MMiSSBundle.hs into our own Bundle type
   described in MMiSSRequest.hs.

   It also contains the (much simpler) functions for converting to
   the ExportOpts required for the <getObject> element,
   and for converting to and from MMiSSVariantSpec's.
   -}
module MMiSSToFromBundle(
   toBundle, -- :: Block -> MMiSSRequest.Bundle -> WithError MMiSSBundle.Bundle
   fromBundle, -- :: MMiSSBundle.Bundle -> BlockM MMiSSRequest.Bundle

   toExportOpts, -- :: MMiSSRequest.GetObject_Attrs -> MMiSSBundle.ExportOpts

   fromVariants, -- :: MMiSSRequest.Variants -> WithError MMiSSVariantSpec
   toVariants, -- :: MMiSSVariantSpec -> MMiSSRequest.Variants
   ) where

import Monad

import Control.Monad.State

import Text.XML.HaXml.OneOfN

import Computation
import ExtendedPrelude
import Bytes(Byte)
import IntPlus

import XmlExtras
import MMiSSVariant

import MMiSSRequest
import MMiSSFormat
import MMiSSBundle
import MMiSSBundleUtils
import MMiSSAPIBlock

-- ---------------------------------------------------------------------------
-- Type declarations
-- ---------------------------------------------------------------------------

-- The BlockM monad is used to thread block accesses when we are converting
-- a bundle to an MMiSSRequest.Files.
type BlockM = State Block

-- ---------------------------------------------------------------------------
-- Converting to and from Bundle's.
-- ---------------------------------------------------------------------------

toBundle :: Block -> MMiSSRequest.Bundle -> WithError MMiSSBundle.Bundle
toBundle block (MMiSSRequest.Bundle (l0 :: [Bundle_])) =
   do
      (l1 :: [(MMiSSBundle.PackageId,BundleNode)]) <- mapM
         (\ (Bundle_ packageId file) ->
            do
               bundleNode <- toBundleNode block file
               return (MMiSSBundle.PackageId (packageIdId packageId),
                  bundleNode)
            )
         l0
      return (MMiSSBundle.Bundle l1)
           

fromBundle :: MMiSSBundle.Bundle -> BlockM MMiSSRequest.Bundle
fromBundle (MMiSSBundle.Bundle (l0 :: [(MMiSSBundle.PackageId,BundleNode)])) =
   do
      (l1 :: [Bundle_]) <- mapM
         (\ (packageId,bundleNode) ->
            do
               file <- fromBundleNode bundleNode
               return (Bundle_ 
                  (MMiSSRequest.PackageId (packageIdStr packageId))
                  file
                  )
            ) 
         l0
      return (MMiSSRequest.Bundle l1)


-- ---------------------------------------------------------------------------
-- Converting to and from lists of BundleNode's.
-- ---------------------------------------------------------------------------

toBundleNodes :: Block -> MMiSSRequest.Files 
   -> WithError [MMiSSBundle.BundleNode]
toBundleNodes block (Files files) =
   do
      (nodes :: [BundleNode]) <- mapM (toBundleNode block) files
      return nodes

fromBundleNodes :: [MMiSSBundle.BundleNode] -> BlockM MMiSSRequest.Files
fromBundleNodes (bundleNodes :: [BundleNode]) =
   do
      files <- mapM fromBundleNode bundleNodes
      return (Files files) 

-- ---------------------------------------------------------------------------
-- Converting to and from BundleNode
-- ---------------------------------------------------------------------------

toBundleNode :: Block -> MMiSSRequest.File 
   -> WithError (MMiSSBundle.BundleNode)
toBundleNode block (File fileLoc0 oneOfOpt) =
   case oneOfOpt of
      Nothing -> return (mkBundleNode NoData)
      Just (OneOf2 (FileVariants variants)) ->
         do
            (vList :: [(Maybe MMiSSVariantSpec,BundleText)]) <-
               mapM
                  (\ (FileVariant variantsOpt contents) ->
                     do
                        vSpecOpt <- case variantsOpt of
                           Nothing -> return Nothing
                           Just variants -> 
                              do
                                 variantSpec <- fromVariants variants
                                 return (Just variantSpec)
                        bundleText <- toBundleText block contents
                        return (vSpecOpt,bundleText)
                     )
                  variants
            return (mkBundleNode (Object vList))
      Just (TwoOf2 files) ->
         do
            bundleNodes <- toBundleNodes block files 
            return (mkBundleNode (Dir bundleNodes))      
   where
      fileLoc1 = toFileLoc fileLoc0

      mkBundleNode :: BundleNodeData -> BundleNode
      mkBundleNode bundleNodeData1 = BundleNode {
         fileLoc = fileLoc1,
         bundleNodeData = bundleNodeData1
         }
  
fromBundleNode :: MMiSSBundle.BundleNode -> BlockM MMiSSRequest.File
fromBundleNode (BundleNode {
      fileLoc = fileLoc0,bundleNodeData = bundleNodeData0}) =
   case bundleNodeData0 of
      NoData -> return (mkFile Nothing)
      Object vList ->
         do
            (variants :: [FileVariant]) <- 
               mapM
                  (\ (vSpecOpt,bundleText) ->
                     do
                        let
                           variantsOpt = fmap toVariants vSpecOpt
                        contents <- fromBundleText bundleText
                        return (FileVariant variantsOpt contents)
                     )
                  vList
            return (mkFile (Just (OneOf2 (FileVariants variants))))
      Dir bundleNodes ->
         do
            files <- fromBundleNodes bundleNodes
            return (mkFile (Just (TwoOf2 files))) 
   where
      fileLoc1 = fromFileLoc fileLoc0

      mkFile fileData = File fileLoc1 fileData
-- ---------------------------------------------------------------------------
-- Converting to and from MMiSSVariantSpec
-- ---------------------------------------------------------------------------

fromVariants :: MMiSSRequest.Variants -> WithError MMiSSVariantSpec
fromVariants (Variants variants) =
   do
      let
         strs = map
            (\ var -> (variantKey var,variantValue var))
            variants
      toMMiSSVariantSpec strs
      
toVariants :: MMiSSVariantSpec -> MMiSSRequest.Variants
toVariants variantSpec =
   let
      strs = fromMMiSSVariantSpec variantSpec
   in
      Variants (
         map 
            (\ (key0,value0) 
               -> Variant {variantKey = key0,variantValue = value0})
            strs
         )

-- ---------------------------------------------------------------------------
-- Converting to and from BundleText.  This only gets really interesting when
-- it's an Element.
-- ---------------------------------------------------------------------------

toBundleText :: Block -> Maybe MMiSSRequest.FileContents 
   -> WithError MMiSSBundle.BundleText
toBundleText block ftOpt =
   case ftOpt of
      Just ft ->
         case readCheck dbnStr of
            Just (blockNo :: Int) ->
               case lookupBlockData block blockNo of
                  Just blockData | blockType blockData == blockType1
                     -> return (
                        BundleString {
                           contents = blockText blockData,
                           charType = toCharType (fromDefaultable (
                              fileContentsCharType ft))
                           }
                        )
                  Nothing -> fail ("Block " ++ show blockNo ++ " not found")
            Nothing -> fail ("Can't parse block number " ++ dbnStr)
         where
            dbnStr = fileContentsDataBlock ft
      Nothing -> return NoText

fromBundleText :: MMiSSBundle.BundleText 
   -> BlockM (Maybe MMiSSRequest.FileContents)
fromBundleText bt = case bundleToICSL bt of
   Nothing -> return Nothing
   Just (icsl,ct) ->
      do
         block0 <- get
         let
            (block1,icslNo) = addBlockData block0 
               (BlockData {
                  blockType = blockType1,
                  blockText = icsl
                  })
         put block1      
         return (Just (FileContents {
            fileContentsDataBlock = show icslNo,
            fileContentsCharType = toDefaultable FileContents_charType_unicode
               (fromCharType ct)
            }))

blockType1 :: Byte
blockType1 = 1

-- ---------------------------------------------------------------------------
-- Converting to and from CharType
-- ---------------------------------------------------------------------------

toCharType :: MMiSSRequest.FileContents_charType -> MMiSSBundle.CharType
toCharType ct = case ct of
   FileContents_charType_byte    -> Byte
   FileContents_charType_unicode -> Unicode

fromCharType :: MMiSSBundle.CharType -> MMiSSRequest.FileContents_charType
fromCharType ct = case ct of
   Byte    -> FileContents_charType_byte    
   Unicode -> FileContents_charType_unicode
    
-- ---------------------------------------------------------------------------
-- Converting to and from FileLocs
-- ---------------------------------------------------------------------------

toFileLoc :: MMiSSRequest.FileLocation -> MMiSSBundle.FileLoc
toFileLoc (FileLocation (onOpt) ot) =
   FileLoc {
      name = fmap (\ (ObjectName s) -> s) onOpt,
      objectType = toBundleType ot
      }

fromFileLoc :: MMiSSBundle.FileLoc -> MMiSSRequest.FileLocation
fromFileLoc (FileLoc {name = nOpt,objectType = objectType}) =
   FileLocation
      (fmap ObjectName nOpt)
      (fromBundleType objectType)

-- ---------------------------------------------------------------------------
-- Converting to and from BundleTypes
-- ---------------------------------------------------------------------------

toBundleType :: MMiSSRequest.ObjectType -> MMiSSBundle.BundleType
toBundleType ot =
   BundleType {
      base = toBundleTypeEnum (objectTypeBaseType ot),
      ext = objectTypeExtType ot,
      extra = objectTypeExtraType ot
      }

fromBundleType :: MMiSSBundle.BundleType -> MMiSSRequest.ObjectType
fromBundleType bt =
   ObjectType {
      objectTypeBaseType = fromBundleTypeEnum (base bt),
      objectTypeExtType = ext bt,
      objectTypeExtraType = extra bt
      }

-- ---------------------------------------------------------------------------
-- Converting to and from BundleTypeEnums
-- ---------------------------------------------------------------------------

toBundleTypeEnum :: MMiSSRequest.ObjectType_baseType ->
   MMiSSBundle.BundleTypeEnum
toBundleTypeEnum bt = case bt of
   ObjectType_baseType_folder -> FolderEnum 
   ObjectType_baseType_plainFile -> FileEnum
   ObjectType_baseType_mmissFolder -> MMiSSFolderEnum
   ObjectType_baseType_mmissObject -> MMiSSObjectEnum 
   ObjectType_baseType_mmissFile -> MMiSSFileEnum
   ObjectType_baseType_mmissPreamble -> MMiSSPreambleEnum

fromBundleTypeEnum :: MMiSSBundle.BundleTypeEnum 
   -> MMiSSRequest.ObjectType_baseType
fromBundleTypeEnum bte = case bte of 
   FolderEnum        -> ObjectType_baseType_folder        
   FileEnum          -> ObjectType_baseType_plainFile     
   MMiSSFolderEnum   -> ObjectType_baseType_mmissFolder   
   MMiSSObjectEnum   -> ObjectType_baseType_mmissObject   
   MMiSSFileEnum     -> ObjectType_baseType_mmissFile     
   MMiSSPreambleEnum -> ObjectType_baseType_mmissPreamble 

-- ---------------------------------------------------------------------------
-- ExportOpts stuff
-- ---------------------------------------------------------------------------

toExportOpts :: MMiSSRequest.GetObject_Attrs -> MMiSSBundle.ExportOpts
toExportOpts attrs =
   let
      what0 = fromDefaultable (getObjectWhat attrs)
      getText1 = case what0 of
         GetObject_what_locations -> False 
         GetObject_what_everything -> True

      format0 = fromDefaultable (getObjectFormat attrs)
      format1 = case format0 of
         GetObject_format_LaTeX -> LaTeX 
         GetObject_format_XML -> XML

      recurse0 = fromDefaultable (getObjectRecurse attrs)
      recurseDepth1 = case recurse0 of
         GetObject_recurse_justThis -> 1
	 GetObject_recurse_allIncluded -> infinity 
   in
      ExportOpts {
         getText = getText1,
         format = format1,
         recurseDepth = recurseDepth1
         }

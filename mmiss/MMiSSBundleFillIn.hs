{- This module fills in various fields of bundles which might be unset after
   they have been read in. 

   Specifically it 
      (1) parses MMiSSEnum objects (stored as MMiSSFiles with extension "tex" 
          or  "xml").
      (2) fills in FolderEnum extra fields when these are Nothing by
          guessing at a plain Folder or an MMiSS SubFolder (depending on
          whether we are inside a package or not). 
   -}
module MMiSSBundleFillIn(
   fillInBundle, -- :: Maybe EntityName -> Bundle -> IO Bundle
   ) where

import Maybe

import Computation
import AtomString
import Dynamics

import EntityNames

import LaTeXPreamble

import MMiSSFormat
import MMiSSBundle
import MMiSSBundleSimpleUtils
import MMiSSBundleConvert
import MMiSSBundleTypes
import MMiSSElementInfo
import MMiSSImportExportErrors
import MMiSSFileSystemExamples
import MMiSSVariant


-- -----------------------------------------------------------------------
-- fillInBundle
-- -----------------------------------------------------------------------

fillInBundle :: Maybe EntityName -> Bundle -> IO Bundle
fillInBundle packageNameOpt (Bundle bundleItems) =
   do
      (parsedOut :: [(PackageId,ParseNodeOut)]) <-
         mapM
            (\ (packageId,bundleNode) ->
               do
                  let
                     locInfo0 = initialLocInfo packageId
                     locInfo1 = locInfo0 {
                        packageNameOpt1 = packageNameOpt
                        }
                  parsedOut <- parseNode locInfo1 bundleNode
                  return (packageId,parsedOut)
               )
            bundleItems
      bundleItems1 <- mapM
         (\ (packageId,(bundleNodeOpt,_)) ->
            case bundleNodeOpt of
               Nothing -> importExportError ("Package " ++ toString packageId
                  ++ " is not a folder")
               Just bundleNode -> return (packageId,bundleNode)
            )  
         parsedOut
      let
         bundle1 = Bundle bundleItems1

         bundles = concat (map (snd . snd) parsedOut)
       
         bundleWE = mergeBundles (bundle1 : bundles)

      coerceImportExportIO bundleWE
     
-- -----------------------------------------------------------------------
-- Convert embedded XML or TeX files into MMiSSObjects with Elements attached.
-- Also provide put MMiSSPreambles in appropriately.
-- -----------------------------------------------------------------------

type ParseNodeOut = (Maybe BundleNode,[Bundle])

parseNode :: LocInfo -> BundleNode -> IO ParseNodeOut 
parseNode locInfo0 bundleNode =
   case base1 of
      -- recursive cases
      FolderEnum -> isFolder
      MMiSSFolderEnum -> isFolder
      -- interesting cases
      MMiSSPreambleEnum -> isPreamble
      MMiSSFileEnum 
         | bundleType1 == mmissObjectAsXMLType
         -> isEmbeddedObject XML 
      MMiSSFileEnum
         | bundleType1 == mmissObjectAsTeXType
         -> isEmbeddedObject LaTeX
      -- boring cases
      MMiSSFileEnum -> isUnchanged
      FileEnum -> isUnchanged
      UnknownType -> isUnchanged
      MMiSSObjectEnum -> isUnchanged
   where
      data1 = bundleNodeData bundleNode

      isUnchanged :: IO ParseNodeOut
      isUnchanged = return (Just bundleNode,[])

      isFolder :: IO ParseNodeOut
      isFolder = case data1 of
         Object _ -> importExportError 
            "Folder unexpectedly contains object data"
         NoData -> isUnchanged
         Dir bundleNodes0 ->
            do
               let
                  nameOptWE = nameFileLocOpt fileLoc1
               nameOpt <- coerceImportExportIO nameOptWE

               let
                  isPackageFolder = case base1 of
                     MMiSSFolderEnum -> True
                     _ -> False

                  locInfo1WE = subDir nameOpt isPackageFolder locInfo0
               locInfo1 <- coerceImportExportIO locInfo1WE

               (bundleNodes1,bundles) <- parseNodes locInfo1 bundleNodes0
               return (Just (mkNode (Dir bundleNodes1)),bundles)

      isPreamble :: IO ParseNodeOut
      isPreamble =
         do
            variants0 <- getVariants
            variants1 <- mapM
               (\ (vb@(variantSpecOpt,bundleText0)) ->
                  case bundleText0 of
                     BundleDyn {} -> return vb
                     NoText -> importExportError 
                        "Preamble element has no text!"
                     _ ->
                        do
                           let
                              str = bundleTextToString bundleText0
                           let
                              preambleWE = fromStringWE str
                           (preamble :: MMiSSLatexPreamble) 
                              <- coerceImportExportIO preambleWE
                           let
                              bundleText1 = mkBundleText preamble
                           return (variantSpecOpt,bundleText1)
                  )
               variants0

            return (Just (mkNode (Object variants1)),[]) 


      isEmbeddedObject :: Format -> IO ParseNodeOut
      isEmbeddedObject format =
         do
            variants0 <- getVariants
            (bundles :: [Bundle]) <- mapM
               (\ (variantSpecOpt,bundleText0) ->
                  do
                     let
                        objectStr = bundleTextToString bundleText0

                     locInfo1 <- case variantSpecOpt of
                        Nothing -> return locInfo0
                        Just variantSpec ->
                           do
                              let
                                 locInfoWE = setVariants variantSpec locInfo0
                              coerceImportExportIO locInfoWE

                     parseObject locInfo1 format objectStr
                  )
               variants0
            return (Nothing,bundles)

      mkNode :: BundleNodeData -> BundleNode
      mkNode bundleNodeData1 = BundleNode {
         fileLoc = fileLoc2,
         bundleNodeData = bundleNodeData1
         }

      fileLoc1 = fileLoc bundleNode
      bundleType1 = objectType fileLoc1
      base1 = base bundleType1

      fileLoc2 = 
         case (base1,extra bundleType1) of
            (FolderEnum,Nothing) ->
               let
                  bundleType2 = if insidePackageFolder locInfo0
                     then
                        mmissSubFolderType
                     else
                        mmissFolderType
               in
                  fileLoc1 {objectType = bundleType2}
            _ -> fileLoc1

      bundleNodeData1 = bundleNodeData bundleNode

      getVariants :: IO [(Maybe MMiSSVariantSpec,BundleText)]
      getVariants = case bundleNodeData1 of
         Object variants -> return variants
         NoData -> importExportError 
            "Imported object has no attached data"
         Dir _ -> importExportError
            "Imported object has subobjects when it's not a file"

-- Construct a Bundle containing (just) the MMiSS Object which is the
-- result of parsing the given String.
parseObject :: LocInfo -> Format -> String -> IO Bundle
parseObject locInfo format objectStr =
   do
      let
         filePath = "Text in Bundle"
         fileSystem = oneFileFileSystem "Text in Bundle" objectStr
      (bundle,_) 
         <- parseBundle1 (toElementInfo locInfo) format fileSystem filePath
      return bundle

parseNodes :: LocInfo -> [BundleNode] -> IO ([BundleNode],[Bundle]) 
parseNodes locInfo0 bundleNodes0 =
   do
      (parsedOut :: [(Maybe BundleNode,[Bundle])]) <- 
         mapM (parseNode locInfo0) bundleNodes0
      let
         bundleNodes1 = catMaybes (map fst parsedOut)
         bundles1 = concat (map snd parsedOut)
      return (bundleNodes1,bundles1) 
      


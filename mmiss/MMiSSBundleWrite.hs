{- This module puts together all the bundle-filling-in, validation and
   writing functions, to present the general function for writing a bundle.
   -}
module MMiSSBundleWrite(
   writeBundle,
   ) where

import IO
import List
import Maybe

import EntityNames

import View

import LaTeXParser(PackageId(..))

import MMiSSInsertionPoint
import MMiSSBundle
import MMiSSBundleFillIn
import MMiSSImportExportErrors
import MMiSSBundleDissect
import MMiSSBundleValidate
import MMiSSBundleReadFiles
import MMiSSBundleNodeCheckTypes
import MMiSSBundleNodeWrite


-- | Errors are provoked by importExportError, and can be caught that way.
writeBundle :: 
   Bundle 
      -- ^ the bundle
   -> Maybe PackageId 
      -- ^ which node in the bundle to write.  If unset we take the first one.
   -> Maybe FilePath
      -- ^ where to look for additional files.  If unspecified, we don't. 
   -> View  
      -- ^ which view to write the node to
   -> InsertionPoint 
   -> IO ()
writeBundle (Bundle []) _ _ _ _ = 
   importExportError "Attempt to write empty bundle"
writeBundle (bundle0 @ (Bundle ((packageId0,_):_)))
      packageIdOpt filePathOpt view insertionPoint =
   do
      let
         packageId = fromMaybe packageId0 packageIdOpt

      bundle1 <- fillInBundle (toInsertionPointName insertionPoint) bundle0
      coerceImportExportIO (validateBundle bundle1)
      let
         bundle2WE = dissectBundle bundle1
      bundle2 <- coerceImportExportIO bundle2WE
      bundle3 <- case filePathOpt of
         Nothing -> return bundle2
         Just filePath -> readFiles filePath bundle2

      let
         Bundle packageBundleNodes = bundle3

      bundleNode <- case List.lookup packageId packageBundleNodes of
         Just bundleNode -> return bundleNode
         Nothing -> 
            importExportError ("PackageId " ++ packageIdStr packageId
               ++ " not found")

      checkTypesWE <- checkBundleNodeTypes view insertionPoint bundleNode
      coerceImportExportIO checkTypesWE

      writeBundleNode view insertionPoint bundleNode


       
            

module MMiSSGetPut(
   getObject,
   putObject,
   ) where

import Control.Monad.State

import Computation
import AtomString

import EntityNames

import LinkManager
import View

import MMiSSVariant
import MMiSSImportExportBundleNode
import MMiSSBundle

import MMiSSRequest
import MMiSSSessionState
import MMiSSAPIBlock
import MMiSSToFromBundle
import {-# SOURCE #-} MMiSSDoXml

-- ----------------------------------------------------------------------------
-- Getting files
-- --------------------------------------------------------------------------

getObject :: MMiSSSessionState -> GetObject -> Block 
   -> IO (GetObjectResponse,Block)
getObject state (GetObject attrs versionRef fullName variantsOpt) 
      block0 =
   do
      view <- lookupView state versionRef
      linkedObject <- getLinkedObject view fullName
      let
         exportOpts = toExportOpts attrs
   
      variantSearchOpt <- case variantsOpt of
         Nothing -> return Nothing
         Just variants ->
            do
               let
                  variantSpecWE = fromVariants variants
               variantSpec <- coerceWithErrorOrBreakIO ourError variantSpecWE
               return (Just (fromMMiSSSpecToSearch variantSpec))

      bundleNode 
         <- exportBundleNode view linkedObject exportOpts variantSearchOpt
      let 
         (file,block1) = runState (fromBundleNode bundleNode) block0
      return (GetObjectResponse file,block1)

-- ----------------------------------------------------------------------------
-- Putting files
-- --------------------------------------------------------------------------

putObject :: MMiSSSessionState -> PutObject -> Block -> IO PutObjectResponse
putObject state (PutObject versionRef fullName files) block =
   do
      let
         bundleNodeWE = toBundleNode block files 
      bundleNode <- coerceWithErrorOrBreakIO ourError bundleNodeWE

      view <- lookupView state versionRef
      linkedObject <- getLinkedObject view fullName
      importBundleNode view linkedObject bundleNode
      return PutObjectResponse

-- ----------------------------------------------------------------------------
-- Utility Functions
-- ----------------------------------------------------------------------------

getLinkedObject :: View -> ObjectFullName -> IO LinkedObject
getLinkedObject view (ObjectFullName fullNameStr) =
   do
      let
         fullNameWE = fromStringWE fullNameStr
      (fullName :: EntityFullName) 
         <- coerceWithErrorOrBreakIO ourError fullNameWE
      linkedObjectOpt <- lookupLinkedObjectByFullName view fullName
      case linkedObjectOpt of
         Nothing -> ourError ("Object " ++ fullNameStr ++ " not found")
         Just linkedObject -> return linkedObject 

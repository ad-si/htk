module MMiSSGetPut(
   getObject,
   putObject,

   getLinkedObject,
   ) where

import Control.Monad.State

import Computation
import AtomString

import EntityNames

import LinkManager
import View

import MMiSSVariant
import MMiSSImportExportBundle
import MMiSSImportExportErrors

import MMiSSRequest
import MMiSSSessionState
import MMiSSAPIBlock
import MMiSSToFromBundle

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
               variantSpec <- coerceWithErrorOrBreakIO importExportError variantSpecWE
               return (Just (fromMMiSSSpecToSearch variantSpec))

      bundle <- exportBundle view linkedObject exportOpts variantSearchOpt
      let
         (file,block1) = runState (fromBundle bundle) block0
      return (GetObjectResponse file,block1)

-- ----------------------------------------------------------------------------
-- Putting files
-- --------------------------------------------------------------------------

putObject :: MMiSSSessionState -> PutObject -> Block -> IO PutObjectResponse
putObject state
      (PutObject versionRef (ObjectFullName fullNameStr) packageIdOpt0 bundle0)
      block =
   do
      let
         fullNameWE = fromStringWE fullNameStr

         packageIdOpt1 = fmap toPackageId packageIdOpt0


      (fullName :: EntityFullName)
         <- coerceWithErrorOrBreakIO importExportError fullNameWE
      let
         bundleWE = toBundle block bundle0
      bundle <- coerceWithErrorOrBreakIO importExportError bundleWE

      view <- lookupView state versionRef
      importBundle view fullName packageIdOpt1 bundle
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
         <- coerceWithErrorOrBreakIO importExportError fullNameWE
      linkedObjectOpt <- lookupLinkedObjectByFullName view fullName
      case linkedObjectOpt of
         Nothing -> importExportError ("Object " ++ fullNameStr
            ++ " not found")
         Just linkedObject -> return linkedObject

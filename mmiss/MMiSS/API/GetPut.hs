module MMiSS.API.GetPut(
   getObject,
   putObject,

   getLinkedObject,
   ) where

import Control.Monad.State

import Util.Computation
import Util.AtomString

import Imports.EntityNames

import Types.LinkManager
import Types.View

import MMiSS.Variant
import MMiSS.ImportExportBundle
import MMiSS.ImportExportErrors

import MMiSS.API.Request
import MMiSS.API.SessionState
import MMiSS.API.Block
import MMiSS.API.ToFromBundle

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

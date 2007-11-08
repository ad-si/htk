-- | This module contains the basic functions for writing and reading an object
-- from the repository.
module MMiSSReadObject(
   simpleReadFromMMiSSObject,
   readMMiSSObject,

   ) where

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import IntPlus
import AtomString(toString)
import Sources (readContents)
import Messages

import View
import Link
import LinkManager
import EntityNames

import Text.XML.HaXml.Types

import MMiSSVariant
import MMiSSElementInfo
import MMiSSVariantObject
import MMiSSObjectType
import MMiSSObjectTypeInstance
import MMiSSReAssemble
import MMiSSPackageFolder
import MMiSSBundleUtils

import {-# SOURCE #-} MMiSSExportFiles


-- | Retrieve a single MMiSSObject\'s data (not any of its children)
simpleReadFromMMiSSObject :: View -> Link MMiSSObject -> MMiSSVariantSearch
   -> IO (WithError (Cache,MMiSSObject))
simpleReadFromMMiSSObject view objectLink variantSearch =
   do
      object <- readLink view objectLink
      cacheOpt <- lookupVariantObjectCache (variantObject object) variantSearch
      case cacheOpt of
         Nothing ->
            do
               title <- getFullName view object
               return (hasError ("No matching variant found for "++title))
         Just cache
            -> return (hasValue (cache,object))

-- | Retrieve an object, expanding includes to a certain depth.  We also
-- get links to all preambles and ExportFiles.
--
-- If allowNotFound is set, not found messages cause us to put up a warning
-- window, otherwise they will be fatal.
--
-- If the VariantSearch is given we use that, otherwise we take the object\'s
-- current one.
readMMiSSObject :: View -> Link MMiSSObject -> Maybe MMiSSVariantSearch
   -> IntPlus -> Bool
   -> IO (WithError (Element,[MMiSSPackageFolder],ExportFiles))
readMMiSSObject view link variantSearchOpt depth0 allowNotFound =
   addFallOutWE (\ break ->
      do
         if depth0 < 1
            then
               break ("Depth given as "++show depth0
                  ++" but must be at least 1")
            else
               done

         -- To gather all the preambles we put them in this MVar.
         (packageFoldersMVar :: MVar [MMiSSPackageFolder]) <- newMVar []

         -- And to gather all ExportFiles we put them here:
         (exportFilesMVar :: MVar ExportFiles) <- newMVar []

         let
            -- getElement is the first function to be passed to
            -- MMiSSReAssemble.reAssembleNoRecursion.

            -- The MMiSSPackageFolder is the folder to look from.
            -- The EntitySearchName specifies what to look for.
            -- The IntPlus is the depth to go down.

            -- We use the hackWithError mechanism to deal with not-found cases,
            -- and split the function into two, to avoid the indentation depth
            -- becoming unbearable.
            getElement :: EntitySearchName -> MMiSSVariantSearch
               -> (MMiSSPackageFolder,IntPlus)
               -> IO (WithError (Maybe (Element,(MMiSSPackageFolder,IntPlus))))
            getElement entitySearchName variantSearch searchData =
               do
                  weData
                     <- getElement0 entitySearchName variantSearch searchData
                  hackWithError Nothing allowNotFound weData

            getElement0 :: EntitySearchName -> MMiSSVariantSearch
               -> (MMiSSPackageFolder,IntPlus)
               -> IO (WithError (Maybe (Element,(MMiSSPackageFolder,IntPlus))))
            getElement0 entitySearchName variantSearch (_,0)
               = return (hasValue Nothing)
            getElement0 entitySearchName variantSearch (packageFolder0,depth) =
               addFallOutWE (\ break ->
                  do
                     let
                        hackBreak = break . hackMess

                     linkOptWE <- lookupMMiSSObject view packageFolder0
                        entitySearchName

                     link <- case fromWithError linkOptWE of
                        Right (Just link) -> return link
                        Right Nothing -> hackBreak ("Object "
                           ++ toString entitySearchName ++ " does not exist")
                        Left mess -> break mess

                     objectDataWE
                        <- simpleReadFromMMiSSObject view link variantSearch

                     (cache,object)
                        <- coerceWithErrorOrBreakIO hackBreak objectDataWE

                     packageFolder1WE <- getMMiSSPackageFolder view
                        (toLinkedObject object)
                     packageFolder1
                        <- coerceWithErrorOrBreakIO break packageFolder1WE

                     modifyMVar_ packageFoldersMVar
                        (\ packageFolders
                           -> return (packageFolder1 : packageFolders)
                           )

                     let
                        element0 = cacheElement cache

                     element1 <- setPackageIdFromFolder
                        view (Just packageFolder0) packageFolder1 element0

                     return (Just (element1,(packageFolder1,depth - 1)))
                  )


            -- doFile is the second function to be passed to
            -- reAssembleNoRecursion.
            doFile :: MMiSSVariantSearch -> (MMiSSPackageFolder,IntPlus)
               -> EntityFullName -> IO ()
            doFile variantSearch0 (packageFolder0,_) file =
               modifyMVar_ exportFilesMVar
                  (return . ((packageFolder0,file,variantSearch0) :))

         -- Construct the top data for reAssembleNoRecursion.

         -- we need to get the object's package folder and a search name for
         -- it.
         object <- readLink view link
         packageFolderWE <- getMMiSSPackageFolder view (toLinkedObject object)
         packageFolder <- coerceWithErrorOrBreakIO break packageFolderWE

         objectNameOpt <- readContents (getLinkedObjectTitleOpt
            (toLinkedObject object))
         objectName <- case objectNameOpt of
            Nothing -> break "Attempt to read deleted object?"
            Just objectName -> return objectName
         let
            searchName = FromAbsolute (EntityFullName [objectName])

         thisVariantSearch <- case variantSearchOpt of
            Just thisVariantSearch -> return thisVariantSearch
            Nothing -> getCurrentVariantSearch (variantObject object)

         elementWE <- reAssembleNoRecursion getElement doFile searchName
            thisVariantSearch (packageFolder,depth0)

         element0 <- coerceWithErrorOrBreakIO break elementWE

         -- The top element in element0 won't have a packageId if it
         -- came from packageFolder, but we put one in anyway, even though
         -- that may well be unnecessary.


         packageIdOpt <- coerceWithErrorOrBreakIO break (getPackageId element0)

         element1 <- case packageIdOpt of
            Nothing ->
               do
                  packageId <- mkPackageId view (toLinkedObject packageFolder)
                  return (setPackageId element0 packageId)
            Just _ -> return element0

         packageFolders1 <- takeMVar packageFoldersMVar
         let
            packageFolders =
               uniqOrdByKey
                  toLinkedObject
                  packageFolders1

         exportFiles <- takeMVar exportFilesMVar
         return (element1,packageFolders,exportFiles)
      )

-- | hackWithError allows us to use the break mechanism to pass back non-fatal
-- error messages by passing back apparently fatal ones but prefixed with a
-- special null character.
--
-- If the Bool is False this \*disables\* this mechanism, simply removing the
-- null character.
hackWithError :: a -> Bool -> WithError a -> IO (WithError a)
hackWithError a enable aWE = case fromWithError aWE of
   Right _ -> return aWE
   Left ('\0':mess) ->
      if enable
         then
            do
               warningMess mess
               return (hasValue a)
         else
            return (hasError mess)
   Left _ -> return aWE

hackMess :: String -> String
hackMess = ( '\0' :)

setPackageIdFromFolder
   :: View -> Maybe MMiSSPackageFolder -> MMiSSPackageFolder -> Element
   -> IO Element
setPackageIdFromFolder view parentPackageFolderOpt thisPackageFolder element0 =
   if parentPackageFolderOpt == Just thisPackageFolder
      then
         return element0
      else
         do
            thisPackageId
               <- mkPackageId view (toLinkedObject thisPackageFolder)
            return (setPackageId element0 thisPackageId)

{- This module contains the basic functions for writing and reading an object
   from the repository. -}
module MMiSSReadObject(
   simpleReadFromMMiSSObject,
   readMMiSSObject,

   ) where
#include "config.h"

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import IntPlus
import AtomString(toString)
import Sources (readContents)

import DialogWin

import View
import Link
import ObjectTypes
import LinkManager
import EntityNames

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import MMiSSVariant
import MMiSSPreamble
import MMiSSVariantObject
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSObjectTypeInstance
import MMiSSReAssemble
import MMiSSPackageFolder


---
-- Retrieve a single MMiSSObject's data (not any of its children)
simpleReadFromMMiSSObject :: View -> Link MMiSSObject -> MMiSSVariantSearch
   -> IO (WithError (Variable,MMiSSObject))
simpleReadFromMMiSSObject view objectLink variantSearch =
   do
      object <- readLink view objectLink
      variableOpt <- lookupVariantObject (variantObject object) variantSearch
      case variableOpt of
         Nothing -> 
            do
               title <- nodeTitleIOPrim object
               return (hasError ("No matching variant found for "++title))
         Just variable -> return (hasValue (variable,object))

---
-- Retrieve an object, expanding includes to a certain depth.  We also 
-- get links to all preambles.
--
-- If allowNotFound is set, not found messages cause us to put up a warning
-- window, otherwise they will be fatal.
--
-- If the VariantSearch is given we use that, otherwise we take the object's
-- current one.
readMMiSSObject :: View -> Link MMiSSObject -> Maybe MMiSSVariantSearch
   -> IntPlus -> Bool -> IO (WithError (Element,[Link MMiSSPreamble]))
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
         (preambleLinksMVar :: MVar [Link MMiSSPreamble]) <- newMVar []

         let
            -- getElement is the function to be passed to
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
            getElement0 entitySearchName variantSearch (packageFolder,depth) =
               addFallOutWE (\ break ->
                  do
                     let
                        hackBreak = break . hackMess

                     linkOptWE <- lookupMMiSSObject view packageFolder 
                        entitySearchName

                     link <- case fromWithError linkOptWE of
                        Right (Just link) -> return link
                        Right Nothing -> hackBreak ("Object "
                           ++ toString entitySearchName ++ " does not exist")
                        Left mess -> break mess
                          
                     objectDataWE 
                        <- simpleReadFromMMiSSObject view link variantSearch

                     (variable,object)
                        <- coerceWithErrorOrBreakIO hackBreak objectDataWE

                     cache <- converter view (linkedObject object) variable

                     packageFolderWE <- getMMiSSPackageFolder view
                        (toLinkedObject object) 
                     packageFolder 
                        <- coerceWithErrorOrBreakIO break packageFolderWE

                     modifyMVar_ preambleLinksMVar 
                        (\ preambleLinks ->
                           return (toMMiSSPreambleLink packageFolder 
                              : preambleLinks)
                           )

                     return (Just (cacheElement cache,
                        (packageFolder,depth - 1)))
                  )

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
            searchName = FromHere (EntityFullName [objectName])

         thisVariantSearch <- case variantSearchOpt of
            Just thisVariantSearch -> return thisVariantSearch
            Nothing -> getCurrentVariantSearch (variantObject object)

         elementWE <- reAssembleNoRecursion getElement searchName
            thisVariantSearch (packageFolder,depth0)

         element <- coerceWithErrorOrBreakIO break elementWE

         preambleLinks <- takeMVar preambleLinksMVar
         return (element,uniqOrd preambleLinks)
      )
         
---
-- hackWithError allows us to use the break mechanism to pass back non-fatal
-- error messages by passing back apparently fatal ones but prefixed with a
-- special null character.
--
-- If the Bool is False this *disables* this mechanism, simply removing the
-- null character.
hackWithError :: a -> Bool -> WithError a -> IO (WithError a)
hackWithError a enable aWE = case fromWithError aWE of
   Right _ -> return aWE
   Left ('\0':mess) ->
      if enable 
         then
            do
               createWarningWin mess []
               return (hasValue a)
         else
            return (hasError mess)
   Left _ -> return aWE

hackMess :: String -> String
hackMess = ( '\0' :)
{- This module contains the basic functions for writing and reading an object
   from the repository. -}
module MMiSSReadObject(
   simpleReadFromMMiSSObject,
   readMMiSSObject,
   getMMiSSObjectLink,

   ) where
#include "config.h"

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import IntPlus
import AtomString(toString)

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

---
-- Get an object's link.
getMMiSSObjectLink :: LinkEnvironment -> EntityFullName 
   -> IO (WithError (Link MMiSSObject))
getMMiSSObjectLink linkEnvironment fullName =
   do
      objectLinkOptWE <- lookupObject linkEnvironment fullName
      case fromWithError objectLinkOptWE of
         Left mess -> return (hasError (
            "Object " ++ toString fullName ++ " is not an MMiSS object"))
         Right Nothing -> return (hasError (
            "Object " ++ toString fullName ++ " does not exist"))
         Right (Just objectLink) -> return (hasValue objectLink)

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

            -- The LinkEnvironment and EntityFullName themselves specify
            -- how the object is to be found; the IntPlus is the depth to
            -- go down.

            -- We use the hackWithError mechanism to deal with not-found cases,
            -- and split the function into two, to avoid the indentation depth
            -- becoming unbearable.
            getElement :: EntityFullName -> MMiSSVariantSearch 
               -> (LinkEnvironment,IntPlus)
               -> IO (WithError (Maybe (Element,(LinkEnvironment,IntPlus))))
            getElement entityFullName variantSearch searchData =
               do
                  weData <- getElement0 entityFullName variantSearch searchData
                  hackWithError Nothing allowNotFound weData

            getElement0 :: EntityFullName -> MMiSSVariantSearch 
               -> (LinkEnvironment,IntPlus)
               -> IO (WithError (Maybe (Element,(LinkEnvironment,IntPlus))))
            getElement0 entityFullName variantSearch (_,0) 
               = return (hasValue Nothing)
            getElement0 entityFullName variantSearch (linkEnvironment,depth) =
               addFallOutWE (\ break ->
                  do
                     let
                        hackBreak = break . hackMess

                     linkOptWE <- lookupObject linkEnvironment entityFullName
                     link <- case fromWithError linkOptWE of
                        Left _ -> break ("Object "++toString entityFullName
                           ++ " is not an MMiSSObject")
                        Right Nothing -> 
                           hackBreak ("Object "++toString entityFullName
                              ++" could not be found")
                        Right (Just link) -> return link
     
                     
                     objectDataWE 
                        <- simpleReadFromMMiSSObject view link variantSearch

                     (variable,object)
                        <- coerceWithErrorOrBreakIO hackBreak objectDataWE

                     cache <- converter view (linkedObject object) variable

                     modifyMVar_ preambleLinksMVar 
                        (\ preambleLinks ->
                           return (cachePreamble cache : preambleLinks))  

                     return (Just (cacheElement cache,
                        (cacheLinkEnvironment cache,depth - 1)))
                  )

         -- Construct the top data for reAssembleNoRecursion.

         -- We construct a special LinkEnvironment to find the top object.
         object <- readLink view link
         linkEnvironment 
            <- newLinkEnvironment (linkedObject object) trivialPath

         thisVariantSearch <- case variantSearchOpt of
            Just thisVariantSearch -> return thisVariantSearch
            Nothing -> getCurrentVariantSearch (variantObject object)

         elementWE <- reAssembleNoRecursion getElement trivialFullName
            thisVariantSearch (linkEnvironment,depth0)

         element <- coerceWithErrorOrBreakIO break elementWE

         preambleLinks <- takeMVar preambleLinksMVar
         return (element,preambleLinks)
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
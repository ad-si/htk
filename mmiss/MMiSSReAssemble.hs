{- This module contains the code for reassembling a document from its
   constituent elements. -} 
module MMiSSReAssemble(
   reAssemble,
   reAssembleNoRecursion,
   ) where

#include "config.h"

import Computation
import AtomString

import EntityNames

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import MMiSSDTDAssumptions
import MMiSSVariant

---
-- The action to extract the Element is allowed to return Nothing.  This 
-- means don't expand.   It will provoke an error if it does this for the
-- very top element however, since then we have no Element to return.
--
-- The search data is threaded down through containing elements.
-- So for example if getElement name1 search1 generates (element2,search2),
-- we apply getElement again on names contained in element2 with search2.
--
-- MMiSSObjects.printAction uses a horrible trick which assumes that the
-- children of an Element are visited in the order supplied.
--
-- MMiSSObjects.printAction and MMiSSReadObject.readObject both assume that
-- the first time we call getElement is on the head element.  But that's
-- OK, as we couldn't do it any other way.
reAssemble :: 
   (EntitySearchName -> MMiSSVariantSearch -> searchData 
   -> IO (WithError (Maybe (Element,searchData)))) 
   -> EntitySearchName -> MMiSSVariantSearch -> searchData
   -> IO (WithError Element)
reAssemble 
      (getElement :: EntitySearchName -> MMiSSVariantSearch -> searchData 
         -> IO (WithError (Maybe (Element,searchData)))) 
      (topName :: EntitySearchName)
      (topVariantSearch :: MMiSSVariantSearch) 
      (topSearchData :: searchData)
      =
   -- We make heavy use of Computation.MonadWithError here 
   let
      getElementWE :: EntitySearchName -> MMiSSVariantSearch -> searchData -> 
         MonadWithError IO (Maybe (Element,searchData))
      getElementWE searchName variantSearch searchData 
         = MonadWithError (getElement searchName variantSearch searchData)

      -- reAssembleWhole does the whole reassembly of the top element.
      MonadWithError (reAssembleWhole :: IO (WithError (Maybe Element))) 
         = reAssembleName topName topVariantSearch topSearchData
            (\ _ -> hasValue ())
      
      -- Reassemble an element designated by name, returning Nothing if
      -- no element of this name can be found.
      -- The last argument designates a check to be carried out
      -- (for example, to determine if the element has appropriate type).
      reAssembleName :: EntitySearchName -> MMiSSVariantSearch -> searchData
         -> (Element -> WithError ()) -> MonadWithError IO (Maybe Element)
      reAssembleName searchName variantSearch0 searchData0 check =
         do
            elementOpt <- getElementWE searchName variantSearch0 searchData0
            case elementOpt of
               Nothing -> return Nothing
               Just (element0,searchData1) ->
                  do
                     monadifyWithError (check element0)

                     -- Set the Element to have the original EntitySearchName.
                     let
                        element1 = setLabel element0 searchName

                     element2 <- reAssembleElement variantSearch0 
                        searchData1 element1
                     return (Just element2)

      -- Reassemble an element, which is not itself an include.
      reAssembleElement :: MMiSSVariantSearch -> searchData -> Element 
         -> MonadWithError IO Element
      reAssembleElement variantSearch0 searchData0 
            (Elem name attributes contents0) =
         do
            let
               attributesSpec :: MMiSSVariantSpec
               attributesSpec = toMMiSSVariantSpecFromXml attributes

               variantSearch1 :: MMiSSVariantSearch
               variantSearch1 
                  = refineVariantSearch variantSearch0 attributesSpec

               doContent :: Content -> MonadWithError IO Content 
               doContent content =
                  case content of
                     CElem element0 -> case unclassifyElement element0 of
                        Nothing -> return content
                        Just (referredNameString,linkAttributes,check) ->
                           do
                              (referredName :: EntitySearchName)
                                 <- monadifyWithError (
                                    fromStringWE referredNameString)
                              let
                                 priority = getPriority element0

                                 linkAttributesSpec 
                                    = toMMiSSVariantSpecFromXml linkAttributes

                                 variantSearch2 = refineVariantSearch
                                    variantSearch1 linkAttributesSpec
                              elementOpt <- reAssembleName referredName
                                 variantSearch2 searchData0 check
                              case elementOpt of
                                 Just element1 -> 
                                    let
                                       element2 = setPriority element1 priority
                                    in
                                       return (CElem element2)
                                 Nothing -> return content
                     _ -> return content

            contents1 <- mapM doContent contents0
            return (Elem name attributes contents1)
   in
      do
         elementOptWE <- reAssembleWhole
         return (mapWithError'
            (\ elementOpt -> case elementOpt of
               Nothing -> hasError ("Element "++toString topName++
                  " is not expanded and so there is nothing to show")
               Just element -> hasValue element
               )
            elementOptWE
            )

---
-- reAssembleNoRecursion is like reAssemble, but it also checks that there
-- are no names which are recursively expanded.
reAssembleNoRecursion :: 
   (EntitySearchName -> MMiSSVariantSearch -> searchData 
      -> IO (WithError (Maybe (Element,searchData)))) 
   -> EntitySearchName -> MMiSSVariantSearch -> searchData
   -> IO (WithError Element)
reAssembleNoRecursion getElement entityName variantSearch searchData =
   let
      -- We do this by threading down an additional list containing the
      -- elements with their MMiSSVariantSearch objects already looked up.
      getElement' entityName variantSearch (searchData,alreadyLookedUp) =
         if elem (entityName,variantSearch) alreadyLookedUp 
            then
               return (hasError ("Element "++toString entityName++
                  " directly or indirectly includes itself"))
            else
               do
                  lookedUpWE <- getElement entityName variantSearch searchData
                  return (mapWithError
                     (fmap
                        (\ (element,searchData) -> 
                           (element,(searchData,(entityName,variantSearch)
                              :alreadyLookedUp))
                           )
                        )
                     lookedUpWE
                     )
   in
      reAssemble getElement' entityName variantSearch (searchData,[])
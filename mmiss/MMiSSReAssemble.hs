{- This module contains the code for reassembling a document from its
   constituent elements. -} 
module MMiSSReAssemble(
   reAssemble,
   reAssembleNoRecursion,
   ) where

import Computation
import AtomString

import EntityNames

import Text.XML.HaXml.Types

import MMiSSDTDAssumptions
import MMiSSVariant
import MMiSSElementInfo(changeLabel)

---
-- The first action function is to extract the Element and is allowed to 
-- return Nothing.  This 
-- means don't expand.   It will provoke an error if it does this for the
-- very top element however, since then we have no Element to return.
--
-- The second action function is called for each reference to a graphics
-- file within an element.
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
   -> (MMiSSVariantSearch -> searchData -> EntityFullName -> IO ())
   -> EntitySearchName -> MMiSSVariantSearch -> searchData
   -> IO (WithError Element)
reAssemble 
      (getElement :: EntitySearchName -> MMiSSVariantSearch -> searchData 
         -> IO (WithError (Maybe (Element,searchData))))
      (doFile :: MMiSSVariantSearch -> searchData -> EntityFullName -> IO ())
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

                     -- Set the Element to have the original EntitySearchName,
                     -- with one exception.  If the searchName is FromAbsolute
                     -- we replace FromAbsolute by FromHere.
                     -- Excuse for this: (1) we don't want FromAbsolute turning
                     -- up in anything exported to the outside world;
                     -- (2) If FromAbsolute works, so will FromHere.
                     let
                        searchName2 = case searchName of
                           FromAbsolute fullName -> FromHere fullName
                           _ -> searchName

                     element1 <- monadifyWithError (
                        changeLabel element0 searchName2)

                     element2 <- reAssembleElement variantSearch0 
                        searchData1 element1
                     return (Just element2)

      -- Reassemble an element, which is not itself an include.
      reAssembleElement :: MMiSSVariantSearch -> searchData -> Element 
         -> MonadWithError IO Element
      reAssembleElement variantSearch0 searchData0 
            (element0 @ (Elem name attributes contents0)) =
         do
            let
               attributesSpec :: MMiSSVariantSpec
               attributesSpec = toMMiSSVariantSpecFromXml attributes

               variantSearch1 :: MMiSSVariantSearch
               variantSearch1 
                  = refineVariantSearch variantSearch0 attributesSpec

               doThisFile :: EntityFullName -> IO ()
               doThisFile file = doFile variantSearch0 searchData0 file

               doContents :: [Content] -> MonadWithError IO [Content]
               doContents = mapM doContent

               doContent :: Content -> MonadWithError IO Content 
               doContent content =
                  case content of
                     CElem (element0 @ (Elem tag0 attributes0 contents0))
                           -> case unclassifyElement element0 of
                        Nothing ->
                           do
                              contents1 <- doContents contents0
                              return (CElem (Elem tag0 attributes0 contents1)) 
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

            mapM_
               (\ fileStr0 ->
                   do
                      file0 <- monadifyWithError (fromStringWE fileStr0)
                      toMonadWithError (doThisFile file0)
                   )    
               (getFiles element0)

            contents1 <- doContents contents0
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
   -> (MMiSSVariantSearch -> searchData -> EntityFullName -> IO ())
   -> EntitySearchName -> MMiSSVariantSearch -> searchData
   -> IO (WithError Element)
reAssembleNoRecursion getElement doFile entityName variantSearch searchData =
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

      doFile' variantSearch0 (searchData0,_) str =
         doFile variantSearch0 searchData0 str
   in
      reAssemble getElement' doFile' entityName variantSearch (searchData,[])
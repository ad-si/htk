{- This module contains the code for reassembling a document from its
   constituent elements. -} 
module MMiSSReAssemble(
   reAssemble,
   reAssembleNoRecursion,
   ) where

import XmlTypes

import Computation
import AtomString

import MMiSSDTDAssumptions
import MMiSSPathsSimple

---
-- The action to extract the Element is allowed to return Nothing.  This 
-- means don't expand.   It will provoke an error if it does this for the
-- very top element however, since then we have no Element to return.
--
-- The search data is threaded down through containing elements.
-- So for example if getElement name1 search1 generates (element2,search2),
-- we apply getElement again on names contained in element2 with search2.
reAssemble :: 
   (EntityName -> searchData -> IO (WithError (Maybe (Element,searchData)))) 
   -> EntityName -> searchData
   -> IO (WithError Element)
reAssemble 
      (getElement :: EntityName -> searchData 
         -> IO (WithError (Maybe (Element,searchData)))) 
      (topName :: EntityName) 
      (searchData0 :: searchData)
      =
   -- We make heavy use of Computation.MonadWithError here 
   let
      getElementWE :: EntityName -> searchData -> 
         MonadWithError IO (Maybe (Element,searchData))
      getElementWE entityName searchData 
         = MonadWithError (getElement entityName searchData)

      reAssembleElement :: searchData -> Element -> MonadWithError IO Element
      reAssembleElement searchData0 (Elem name attributes contents0) =
         do
            let
               doContent :: Content -> MonadWithError IO Content 
               doContent content =
                  case content of
                     CElem element -> case unclassifyElement element of
                        Nothing -> return content
                        Just (referredNameString,check) ->
                           do
                              let
                                 referredName = fromString referredNameString
                              elementOpt 
                                 <- getElementWE referredName searchData0
                              case elementOpt of
                                 Nothing -> return content
                                 Just (element0,searchData1) ->
                                    do
                                       monadifyWithError (check element0)
                                       element1 <- reAssembleElement 
                                          searchData1 
                                          element0
                                       return (CElem element1)
                     _ -> return content

            contents1 <- mapM doContent contents0
            return (Elem name attributes contents1)
   in
      do
         -- Handle the top element (this needs to be done specially, since
         -- getElement is not allowed to return Nothing
         topElementOptWE <- getElement topName searchData0
         case fromWithError topElementOptWE of
            Left error -> return (hasError error)
            Right Nothing -> return (hasError ("Element "++toString topName++
               " is not expanded and so there is nothing to show"))
            Right (Just (element,searchData1)) ->
               let
                  (MonadWithError action) 
                     = reAssembleElement searchData1 element
               in
                  action

---
-- reAssembleNoRecursion is like reAssemble, but it also checks that there
-- are no names which are recursively expanded.
reAssembleNoRecursion :: 
   (EntityName -> searchData -> IO (WithError (Maybe (Element,searchData)))) 
   -> EntityName -> searchData
   -> IO (WithError Element)
reAssembleNoRecursion getElement entityName searchData =
   let
      -- We do this by threading down an additional list containing the
      -- elements already looked up.
      getElement' entityName (searchData,alreadyLookedUp) =
         if elem entityName alreadyLookedUp 
            then
               return (hasError ("Element "++toString entityName++
                  " directly or indirectly includes itself"))
            else
               do
                  lookedUpWE <- getElement entityName searchData
                  return (mapWithError
                     (fmap
                        (\ (element,searchData) -> 
                           (element,(searchData,entityName:alreadyLookedUp))
                           )
                        )
                     lookedUpWE
                     )
   in
      reAssemble getElement' entityName (searchData,[])
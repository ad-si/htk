{- This module contains the function for constructing the EmacsFS
   (Emacs Filing System) required by emacs/EmacsEdit.hs, and the
   PrintAction that EmacsEdit requires. -}
module MMiSSEmacsEdit(
   editMMiSSObjectXml,
      -- :: View -> Link MMiSSObject -> IO ()
      -- Edit the object as Xml
   editMMiSSObjectLaTeX,
      -- :: View -> Link MMiSSObject -> IO ()
      -- Edit the object as LaTeX

   ) where

#include "config.h"

import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Exception(assert)
import Data.Set

import Computation
import Thread
import ExtendedPrelude
import AtomString(toString,fromString,fromStringWE)
import ReferenceCount
import Sources

import Lock

import DialogWin

import GraphConfigure

import ViewType
import Link
import EntityNames
import LinkManager
import SpecialNodeActions

import EmacsContent
import EmacsEdit

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import MMiSSDTDAssumptions
import MMiSSFormat
import MMiSSEditFormatConverter
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSVariant
import MMiSSVariantObject(getCurrentVariantSearch)
import MMiSSEditXml(TypedName,toExportableXml)
import MMiSSLaTeX
import MMiSSObjectTypeInstance
import MMiSSWriteObject
import MMiSSReadObject
import MMiSSReAssemble
import MMiSSPreamble


-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

---
-- EditRef is the "ref" type passed to EmacsEdit. 
-- We expect it to be (possibly) made more 
-- complicated later when we handle variant attributes properly (allowing
-- the same object to be edited in multiple variants simultaneously), hence
-- the need to make it abstract.
--
-- The extra fields (apart from the link and variants) mean we can get what's 
-- needed to do the Emacs buttons, on the basis of what's in the referencing 
-- object, without having to dereference the link.
data EditRef = 
      EditRef {
         linkEnvironment :: LinkEnvironment,
         description :: EntityFullName,
         outerVariants :: MMiSSVariantSearch,
            -- ^ The outer variants attached to the document containing this 
            -- link.  It is assumed (or at least hoped) that these will not 
            -- change.
         linkVariants :: MMiSSVariantSpec,
            -- ^ Variants attached to this particular link.  These may somehow
            -- be edited (though there is no facility for doing that at the 
            -- moment). 
         miniType :: Char
         }

compareOpt :: EditRef 
   -> (EntityFullName,LinkEnvironment,MMiSSVariantSearch,MMiSSVariantSpec)
compareOpt editRef = 
   (description editRef,linkEnvironment editRef,
      outerVariants editRef,linkVariants editRef)

instance Eq EditRef where
   (==) = mapEq compareOpt

instance Ord EditRef where
   compare = mapOrd compareOpt

-- ----------------------------------------------------------------------
-- The exported functions
-- ----------------------------------------------------------------------


editMMiSSObjectXml :: View -> Link MMiSSObject -> IO ()
editMMiSSObjectXml = editMMiSSObjectGeneral XML

editMMiSSObjectLaTeX :: View -> Link MMiSSObject -> IO ()
editMMiSSObjectLaTeX = editMMiSSObjectGeneral LaTeX

editMMiSSObjectGeneral :: Format -> View -> Link MMiSSObject -> IO ()
editMMiSSObjectGeneral format
    = editMMiSSObjectInner (toEditFormatConverter format)

editMMiSSObjectInner :: EditFormatConverter -> View -> Link MMiSSObject 
   -> IO ()
editMMiSSObjectInner formatConverter view link =
   do
      object <- readLink view link
      variantSearch <- getCurrentVariantSearch (variantObject object)

      thisInsertionOpt <- getCurrentInsertion (toLinkedObject object)

      case thisInsertionOpt of
         Nothing ->
            do
               createErrorWin (
                  "Object is not current inserted anywhere and cannot be "
                  ++ "edited!") []
               done
         Just insertion ->
            do
               let
                  (parentLinkedObject,name) = unmkInsertion insertion
               linkEnvironment 
                  <- newLinkEnvironment parentLinkedObject trivialPath 
               let
                  emacsFS = mkEmacsFS view formatConverter 
                  printAction = mkPrintAction view formatConverter 

                  topEditRef = EditRef {
                     linkEnvironment = linkEnvironment,
                     outerVariants = variantSearch,
                     linkVariants = emptyMMiSSVariantSpec,
                     miniType = getObjectMiniType object,
                     description = EntityFullName [name]
                     }
            
               editEmacs emacsFS printAction topEditRef

-- ----------------------------------------------------------------------
-- Making the FS
-- ----------------------------------------------------------------------

---
-- This function constructs the file-system itself.
mkEmacsFS :: View -> EditFormatConverter -> EmacsFS EditRef
mkEmacsFS view (EditFormatConverter {toEdit = toEdit,fromEdit = fromEdit}) =
   let
      toDescription :: EditRef -> String
      toDescription = toString . description

      -- Now for the difficult one.
      editFS :: EditRef 
         -> IO (WithError (EmacsContent EditRef,EditedFile EditRef))
      editFS (editRef @ EditRef {linkEnvironment = linkEnvironment0,
            miniType = miniType0,description = description0}) =
         addFallOutWE (\ break -> 
            do
               let
                  name = toString description0

                  variants = toVariants editRef

               objectLinkWE <- getMMiSSObjectLink linkEnvironment0 description0

               objectLink <- coerceWithErrorOrBreakIO break objectLinkWE
         
               -- retrieve the object data.
               objectDataWE <- simpleReadFromMMiSSObject view objectLink
                  variants

               (variable,object) <- coerceWithErrorOrBreakIO break objectDataWE

               let
                  thisElementLink = element variable
                  thisLinkedObject = linkedObject object
                  thisObjectType = mmissObjectType object

               -- get the object's parent.

               if getObjectTypeMiniType thisObjectType == miniType0
                  then
                     done
                  else
                     break ("Object "++name++" has wrong type")
               let
                  lock = editLock variable
 
               isAvailable <- tryAcquire lock
               if isAvailable
                  then
                     done
                  else 
                     break ("Attempt to edit a variant of "++name
                        ++ " when it is already being edited")

               let
                  -- redefine break so that it releases the lock, if something
                  -- goes wrong.  We use a horrible trick to do this when the
                  -- break is evaluated.
                  break2 mess =
                     seq (unsafePerformIO (release lock)) (break mess)

               -- Get the element and its attributes.
               thisElement <- readLink view thisElementLink

               let
                  Elem _ attributes _ = thisElement

               -- Extract a LinkEnvironment for the object.
               cache <- converter view thisLinkedObject variable
               let
                  linkEnvironment1 = cacheLinkEnvironment cache
               
               -- Create variants used for searching in this object,
               -- We refine the existing variants with the one given by the
               -- object's attributes.
               let
                  variantSpec = toMMiSSVariantSpecFromXml attributes

                  -- outerVariants1 will become the outerVariants for contained
                  -- links.
                  outerVariants1 = refineVariantSearch variants variantSpec

                  (contentWE :: WithError (EmacsContent (TypedName,
                     [Attribute]))) = toEdit name thisElement

               (content0 :: EmacsContent (TypedName,[Attribute]))
                  <- coerceWithErrorOrBreakIO break2 contentWE

               -- convert content0 into EmacsContent EditRef.
               content1 <- mapMonadic 
                  (\ ((string,miniType),includeAttributes) ->
                     do
                        (fullName :: EntityFullName) 
                           <- coerceWithErrorOrBreakIO break2 
                              (fromStringWE string)
                        let
                           linkVariants1 = 
                              toMMiSSVariantSpecFromXml includeAttributes

                           editRef =
                              EditRef {
                                 linkEnvironment = linkEnvironment1,
                                 outerVariants = outerVariants1,
                                 linkVariants = linkVariants1,
                                 miniType = miniType,
                                 description =  fullName
                                 }

                        return editRef 
                     )
                  content0
               
               -- We now have to set up the EditedFile stuff 
               let
                  writeData (emacsContent0 :: EmacsContent EditRef) =
                     addFallOutWE (\ break ->
                       do
                          let
                             (emacsContent1 :: EmacsContent (TypedName,
                                   [Attribute])) =
                                fmap
                                   (\ editRef -> 
                                      ((toString (description editRef),
                                         miniType editRef),

                                         fromMMiSSVariantSpecToXml
                                            (linkVariants editRef)
                                         )
                                      )
                                   emacsContent0
                          elementWE <- fromEdit name emacsContent1

                          -- We now play games with the element.  Specifically,
                          -- we change its label to ".", first checking that
                          -- the user hasn't changed it.
                          element0 <- coerceWithErrorOrBreakIO break elementWE
                          let
                             description1WE = getLabel element0

                          description1 <- case fromWithError description1WE of
                             Left _ -> break ("Object "++name
                                ++" has somehow lost its label")
                             Right description1 -> return description1

                          if entityBase description1 == entityBase description0
                             then
                                done
                             else
                                break ("Label of object "++name
                                   ++" has been illegally changed to "
                                   ++toString description1
                                   )

                          let
                             element1 = setLabel element0 trivialFullName

                          linkWE <- writeToMMiSSObject (preamble variable)
                             thisObjectType view thisLinkedObject Nothing
                             element1 False
                          let
                             link = coerceWithErrorOrBreak break linkWE
                          link `seq` done
                          setFontStyle (nodeActions object) 
                             BoldItalicFontStyle
                          createMessageWin 
                             ("Commit of "++name++ " successful!") []
                       )

                  finishEdit =
                     do
                        release lock
                        remEdit object

                  (editedFile :: EditedFile EditRef) = EditedFile {
                     writeData = writeData,
                     finishEdit = finishEdit
                     }

               addEdit object

               return (content1,editedFile)
            )

      emacsFS = EmacsFS {
         editFS = editFS,
         toMiniType = miniType,
         toDescription = toDescription
         }
   in
      emacsFS

---
-- Returns the miniType for an object.
getObjectMiniType :: MMiSSObject -> Char
getObjectMiniType object = getObjectTypeMiniType (mmissObjectType object)

---
-- Returns the miniType for an object type.
getObjectTypeMiniType :: MMiSSObjectType -> Char
getObjectTypeMiniType objectType = getMiniType (xmlTag objectType)

-- --------------------------------------------------------------
-- Functions for changing the border to indicate that an object is
-- being edited, or not.  
-- --------------------------------------------------------------

addEdit :: MMiSSObject -> IO ()
addEdit object =
   do
      addRef (editCount object)
      -- We always set the border.  Of course if it's already been done, that's
      -- harmless
      setBorder (nodeActions object) DoubleBorder

remEdit :: MMiSSObject -> IO ()
remEdit object =
   do
      doUnset <- remRef (editCount object)
      if doUnset
         then
            setBorder (nodeActions object) GraphConfigure.def
         else
            done
      
-- ----------------------------------------------------------------------
-- The PrintAction
-- ----------------------------------------------------------------------


mkPrintAction :: View -> EditFormatConverter -> PrintAction EditRef
mkPrintAction view (EditFormatConverter {fromEdit = fromEdit}) =
   let
      printAction :: EditRef 
         -> (EditRef -> IO (WithError (EmacsContent (Bool,EditRef)))) 
         -> IO ()
      printAction topRef 
            (getContent :: EditRef 
               -> IO (WithError (EmacsContent (Bool,EditRef)))) =
         do
            -- To gather all the preambles we put them in this MVar.
            (preambleLinksMVar :: MVar [Link MMiSSPreamble]) <- newMVar []

            let
               -- Unfortunately reAssemble doesn't do exactly what we want;
               -- it gives us the Element, but what we want is the
               -- (Bool,EditRef).  To work around this we *assume* that
               -- reAssembleArg visits the children of each node in order,
               -- and pass as search data an MVar containing the 
               -- (Bool,EditRef)'s.  The EntityFullName it passes then becomes
               -- irrelevant . . .
               reAssembleArg :: EntityFullName -> MMiSSVariantSearch
                  -> MVar [(Bool,EditRef)] 
                  -> IO (WithError (Maybe (Element,MVar [(Bool,EditRef)])))
               reAssembleArg entityFullName variantSearch0 mVar =
                  do
                     ((doExpand,editRef):rest) <- takeMVar mVar

                     putMVar mVar rest
                     assert (entityFullName == description editRef) done

                     if doExpand
                        then
                           addFallOutWE (\ break ->
                              do
                                 (content0WE :: WithError
                                    (EmacsContent (Bool,EditRef)))
                                    <- getContent editRef

                                 content0 <- coerceWithErrorOrBreakIO break
                                    content0WE

                                 nextMVar <- newMVar (toEmacsLinks content0)

                                 let
                                    (content1 :: EmacsContent (TypedName,
                                          [Attribute]))= fmap 
                                       (\ (b,editRef) -> 
                                          ((toString (description editRef),
                                             miniType editRef),
                                             fromMMiSSVariantSpecToXml (
                                                linkVariants editRef
                                                )
                                             )
                                          )
                                       content0

                                    name = toString entityFullName

                                 elementWE <- fromEdit name content1

                                 element <- coerceWithErrorOrBreakIO break 
                                    elementWE

                                 -- Also retrieve the object's preamble.  This
                                 -- requires us to look at the object again.
                                 preambleLinkWE <- 
                                    getPreambleLink 
                                       view 
                                       (linkEnvironment editRef)
                                       (description editRef)
                                       (toVariants editRef)

                                 preambleLink <- coerceWithErrorOrBreakIO 
                                    break preambleLinkWE
                                     
                                 modifyMVar_ preambleLinksMVar 
                                    (\ preambleLinks ->
                                       return (preambleLink : preambleLinks))  

                                 return (Just (element,nextMVar))
                              )                                  
                        else
                           return (hasValue Nothing)

            topMVar <- newMVar [(True,topRef)]

            let
               variantSearch = refineVariantSearch (outerVariants topRef)
                  (linkVariants topRef)

            elementWE <- reAssemble reAssembleArg 
               (description topRef) variantSearch topMVar
            case fromWithError elementWE of
               Left error -> createErrorWin error []
               Right element ->
                  do
                     -- Extract all preambles
                     preambleLinks <- takeMVar preambleLinksMVar

                     -- We do the actual printing in a separate thread,
                     -- so the user can continue editing.
                     forkIO (
                        do
                           stringWE <- exportElement view LaTeX 
                                 preambleLinks element
                           case fromWithError stringWE of
                              Left error -> createErrorWin error []
                              Right str -> mmissLaTeX (
                                 toString (description topRef)) str
                        )
                     done
   in
      PrintAction printAction

---
-- Extract a link to an object's preamble.
getPreambleLink :: View -> LinkEnvironment -> EntityFullName 
   -> MMiSSVariantSearch -> IO (WithError (Link MMiSSPreamble))
getPreambleLink view linkEnvironment fullName variantSearch =
   addFallOutWE (\ break ->
      do
         objectLinkWE <- getMMiSSObjectLink linkEnvironment fullName
         objectLink <- coerceWithErrorOrBreakIO break objectLinkWE
         objectDataWE
            <- simpleReadFromMMiSSObject view objectLink variantSearch
         (variable,object) <- coerceWithErrorOrBreakIO break objectDataWE
         return (preamble variable)
      )

-- ----------------------------------------------------------------------
-- Other utility functions
-- ----------------------------------------------------------------------

---
-- Return variants corresponding to an EditRef
toVariants :: EditRef -> MMiSSVariantSearch
toVariants editRef =
   refineVariantSearch (outerVariants editRef) (linkVariants editRef)
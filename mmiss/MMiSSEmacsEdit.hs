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

import SimpleForm
import DialogWin
import MenuType

import GraphConfigure

import ViewType
import Link
import EntityNames
import LinkManager
import SpecialNodeActions
import AttributesType

import EmacsContent
import EmacsEdit

import Text.XML.HaXml.Types

import LaTeXParser(MMiSSExtraPreambleData(..))

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
import MMiSSPackageFolder
import {-# SOURCE #-} MMiSSExportFiles


-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

---
-- EditRef is the "ref" type passed to EmacsEdit. 
--
-- The extra fields (apart from the link and variants) mean we can get what's 
-- needed to do the Emacs buttons, on the basis of what's in the referencing 
-- object, without having to dereference the link.
data EditRef = 
      EditRef {
         package :: MMiSSPackageFolder,
         searchName :: EntitySearchName,
           -- ^ "package" and "searchName" represent the way in which the
           -- referenced object is to be searched for, using the
           -- lookupXXX functions in MMiSSPackageFolder.
         outerVariants :: MMiSSVariantSearch,
            -- ^ The outer variants attached to the document containing this 
            -- link.  It is assumed (or at least hoped) that these will not 
            -- change.
         linkVariants :: MMiSSVariantSpec,
            -- ^ Variants attached to this particular link.  These may somehow
            -- be edited (though there is no facility for doing that at the 
            -- moment). 
         priority :: String,
            -- ^ Priority attached to this link.
         miniType :: Char
         }

compareOpt :: EditRef 
   -> (EntitySearchName,MMiSSPackageFolder,MMiSSVariantSearch,MMiSSVariantSpec)
compareOpt editRef = 
   (searchName editRef,package editRef,
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

      packageAndNameWE 
         <- getMMiSSPackageFolderAndName view (toLinkedObject object)

      case fromWithError packageAndNameWE of
         Left mess ->
            do
               createErrorWin mess []
               done
         Right (package,name) ->
            do
               let
                  searchName :: EntitySearchName
                  searchName = FromAbsolute (EntityFullName [name])

                  emacsFS = mkEmacsFS view formatConverter 
                  printAction = mkPrintAction view formatConverter 

                  topEditRef = EditRef {
                     package = package,
                     searchName = searchName,
                     outerVariants = variantSearch,
                     linkVariants = emptyMMiSSVariantSpec,
                     miniType = getObjectMiniType object,
                     priority = "1"
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
      -- toDescription needs to eliminate the FromAbsolute that occurs
      -- for the top edited buffer
      toDescription :: EditRef -> String
      toDescription editRef =
         case searchName editRef of
            FromAbsolute fullName -> toString fullName
            searchName1 -> toString searchName1

      -- Now for the difficult one.
      editFS :: EditRef 
         -> IO (WithError (EmacsContent EditRef,EditedFile EditRef))
      editFS (editRef @ EditRef {
            searchName = searchName0,miniType = miniType0}) =
         addFallOutWE (\ break -> 
            do
               let
                  name = toDescription editRef
                  variants = toVariants editRef

               -- Get the object data
               objectLinkWE <- getEditRef view editRef
               objectLink <- coerceWithErrorOrBreakIO break objectLinkWE
         
               -- retrieve the object data.
               objectDataWE <- simpleReadFromMMiSSObject view objectLink
                  variants

               (variable,object) <- coerceWithErrorOrBreakIO break objectDataWE

               let
                  thisElementLink = element variable
                  thisLinkedObject = linkedObject object
                  thisObjectType = mmissObjectType object

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

               
               -- Extract a parent MMiSSPackageFolder and name for the object,
               -- to be used partly for looking up includes within the object,
               -- and also to provide a static way of writing back to the
               -- object. 
               packageAndName1WE 
                  <- getMMiSSPackageFolderAndName view thisLinkedObject

               (package1,name1) 
                  <- coerceWithErrorOrBreakIO break2 packageAndName1WE
               
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

               let
                  mapContent :: EmacsContent (TypedName,[Attribute]) 
                     -> IO (EmacsContent EditRef)
                  mapContent = mapMonadic
                     (\ ((string,miniType),includeAttributes) ->
                        do
                           (searchName1 :: EntitySearchName) 
                              <- coerceWithErrorOrBreakIO break2 
                                 (fromStringWE string)
                           let
                              linkVariants1 = 
                                 toMMiSSVariantSpecFromXml includeAttributes

                              editRef =
                                 EditRef {
                                    package = package1,
                                    searchName = searchName1, 
                                    outerVariants = outerVariants1,
                                    linkVariants = linkVariants1,
                                    miniType = miniType,
                                    priority = getPriorityAttributes 
                                       includeAttributes
                                    }

                           return editRef 
                        )

               -- convert content0 into EmacsContent EditRef.
               content1 <- mapContent content0 
               
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
                                      ((toString (searchName editRef),
                                         miniType editRef),
                                         setPriorityAttributes'
                                            (fromMMiSSVariantSpecToXml
                                               (linkVariants editRef)
                                               )
                                             (priority editRef)
                                         )
                                      )
                                   emacsContent0

                          elementWE <- fromEdit name emacsContent1
                          element0 <- coerceWithErrorOrBreakIO break elementWE


                          let
                             -- We will write the object back into its
                             -- actual parent folder, with original name.
                             -- This means things won't get upset, even if,
                             -- say, the including object alters its preamble.

                             element1 = setLabel element0
                                (FromAbsolute (EntityFullName [name1]))

                          writeOutWE <- writeToMMiSSObject
                             thisObjectType view package1 Nothing
                             element1 False

                          (link,elementOpt,preObjects) 
                             <- coerceWithErrorOrBreakIO break writeOutWE

                          setFontStyle (nodeActions object) 
                             BoldItalicFontStyle

                          emacsContentOpt <- case elementOpt of
                             Nothing -> return Nothing
                             Just newElement0 ->
                                do
                                   let
                                      newElement1 
                                         = setLabel newElement0 searchName0

                                      contentWE :: WithError 
                                         (EmacsContent (TypedName,[Attribute]))
                                      contentWE = toEdit name newElement1
                                   
                                   case fromWithError contentWE of
                                      Left mess ->
                                         do
                                            createWarningWin
                                               ("Commit of " ++ name 
                                                  ++ " successful, but "
                                                  ++ "attempt to recompute "
                                                  ++ "magic buttons failed:\n"
                                                  ++ mess) []
                                            return Nothing
                                      Right content0 ->
                                         do
                                            content1 <- mapContent content0
                                            return (Just content1)
                               
                          createMessageWin 
                             ("Commit of "++name++ " successful!") []
                          return emacsContentOpt
                       )

                  finishEdit =
                     do
                        release lock
                        remEdit object

                  (editedFile :: EditedFile EditRef) = EditedFile {
                     writeData = writeData,
                     finishEdit = finishEdit
                     }

               return (content1,editedFile)
            )

      createRef :: EditRef -> IO (WithError (Maybe EditRef))
      createRef editRef =
         addFallOutWE (\ break ->
            do
               -- We first attempt to access the outer referenced object, so
               -- we can get at the appropriate package.
               outerObjectLinkWE <- getEditRef view editRef
               outerObjectLink
                  <- coerceWithErrorOrBreakIO break outerObjectLinkWE
               outerObject <- readLink view outerObjectLink
               package1WE <- getMMiSSPackageFolder view outerObject 
               package1 <- coerceWithErrorOrBreakIO break package1WE

               let
                  outerVariants1 = toVariants editRef

                  -- Prompt for the variant, search name and type.  We get the
                  -- search name via an ExtraFormItem
                  form0 :: Form String
                  form0 = newFormEntry "Name" ""

                  form1 :: Form EntitySearchName
                  form1 = mapForm fromStringWE form0

                  form2 :: Form String
                  form2 = newFormOptionMenu (map xmlTag allObjectTypes)

                  form3 :: Form Char
                  form3 = fmap getMiniType form2

                  form4 :: Form String
                  form4 = newFormEntry "Priority" "0"

                  form5 :: Form String
                  form5 = guardForm 
                     (\ priorityStr 
                         -> case readCheck priorityStr of
                           Just (_ :: Double) -> True 
                           _ -> False
                         )
                      "Priority must be a number"
                      form4


                  extraForm :: Form (EntitySearchName,(Char,String))
                  extraForm = form1 // (form3 // form5)

               extraFormItem <- mkExtraFormItem extraForm
               
               attributesOpt <- inputAttributes view variantAttributesType2
                  (Just extraFormItem)
               case attributesOpt of
                  Nothing -> return Nothing
                  Just attributes ->
                     do
                        linkVariants1 
                           <- toMMiSSVariantSpecFromAttributes attributes
                        (searchName1,(miniType1,priority1)) 
                           <- readExtraFormItem extraFormItem
                        let
                           editRef = EditRef {
                              package = package1,
                              searchName = searchName1,
                              outerVariants = outerVariants1,
                              linkVariants = linkVariants1,
                              priority = priority1,
                              miniType = miniType1
                              }

                        return (Just editRef)
            )

      emacsFS = EmacsFS {
         editFS = editFS,
         toMiniType = miniType,
         toDescription = toDescription,
         createRef = createRef
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
mkPrintAction view editFormatConverter =
   let
      printAction :: EditRef 
         -> (EditRef -> IO (WithError (EmacsContent (Bool,EditRef)))) 
         -> IO ()
      printAction topRef 
            (getContent :: EditRef 
               -> IO (WithError (EmacsContent (Bool,EditRef)))) =
         do
            -- To gather all the preambles we put them in this MVar.
            (preambleLinksMVar 
               :: MVar [(Link MMiSSPreamble,MMiSSExtraPreambleData)]) 
               <- newMVar []


            -- To gather the export files we put them in this MVar
            (exportFilesMVar :: MVar ExportFiles)
               <- newMVar []

            topMVar <- newMVar [(True,topRef)]
             

            let
               variantSearch = refineVariantSearch (outerVariants topRef)
                  (linkVariants topRef)

            elementWE <- reAssemble 
               (reAssembleArg view preambleLinksMVar getContent 
                  editFormatConverter) 
               (doFile exportFilesMVar)
               (searchName topRef) variantSearch 
               (package topRef,topMVar)

            case fromWithError elementWE of
               Left error -> createErrorWin error []
               Right element ->
                  do
                     -- Extract all preambles and exportFiles
                     preambleLinks <- takeMVar preambleLinksMVar

                     exportFiles <- takeMVar exportFilesMVar

                     -- We do the actual printing in a separate thread,
                     -- so the user can continue editing.
                     forkIO (
                        do
                           stringWE <- exportElement view LaTeX 
                                 preambleLinks element
                           case fromWithError stringWE of
                              Left error -> createErrorWin error []
                              Right str -> mmissLaTeX view
                                 (toString (searchName topRef)) str
                                 exportFiles
                        )
                     done
   in
      PrintAction printAction

-- Function to be passed as first argument of the reAssemble function.  
-- This needs 
-- four arguments in addition to those provided by reAssemble:
-- the view, the MMiSSPackageFolder used for looking up MMiSSFile's,
-- an MVar for writing preamble links to, the
-- getContent function, and the EditFormatConverter.
--
-- Unfortunately reAssemble doesn't do exactly what we want;
-- it gives us the Element, but what we want is the
-- (Bool,EditRef).  To work around this we *assume* that
-- reAssembleArg visits the children of each node in order,
-- and pass as search data an MVar containing the 
-- (Bool,EditRef)'s.  The EntitySearchName it passes then 
-- becomes irrelevant . . .
reAssembleArg :: View -> MVar [(Link MMiSSPreamble,MMiSSExtraPreambleData)]
   -> (EditRef -> IO (WithError (EmacsContent (Bool,EditRef))))        
   -> EditFormatConverter
   -> EntitySearchName
   -> MMiSSVariantSearch -> (MMiSSPackageFolder,MVar [(Bool,EditRef)]) 
   -> IO (WithError (Maybe (Element,
      (MMiSSPackageFolder,MVar [(Bool,EditRef)]))))
reAssembleArg view preambleLinksMVar getContent editFormatConverter
      entitySearchName variantSearch0 (packageFolder0,mVar) =
   do
      ((doExpand,editRef):rest) <- takeMVar mVar

      putMVar mVar rest
      assert (entitySearchName == searchName editRef) done

      let
         name = toString (searchName editRef)

         packageAct :: IO (WithError (MMiSSPackageFolder,Link MMiSSPreamble))
         packageAct =
            do
               objectLinkWE <- getEditRef view editRef
               mapWithErrorIO'
                  (\ objectLink ->
                     do
                        mmissObject 
                           <- readLink view objectLink
                        packageFolderWE <- getMMiSSPackageFolder
                           view mmissObject
                        return (mapWithError
                           (\ packageFolder 
                              -> (packageFolder,
                                 toMMiSSPreambleLink packageFolder)
                              )
                           packageFolderWE
                           )
                     )
                  objectLinkWE

      if doExpand
         then
            addFallOutWE (\ break ->
               do
                  -- Get the preamble link first of all,
                  -- as if we can't get the preamble we
                  -- can't print the object.
                  preambleLinkWE <- packageAct
                  case fromWithError preambleLinkWE of
                     Left mess ->
                        do
                           createErrorWin (name ++ ": " ++ mess) []
                           return Nothing
                     Right (packageFolder1,preambleLink) ->
                        do
                           (content0WE 
                                 :: WithError (EmacsContent (Bool,EditRef)))
                              <- getContent editRef

                           content0 <- coerceWithErrorOrBreakIO break
                              content0WE

                           nextMVar <- newMVar (toEmacsLinks content0)

                           let
                              content1 :: EmacsContent (TypedName,[Attribute])
                              content1 = fmap 
                                 (\ (b,editRef) -> 
                                    ((toString (searchName editRef),
                                          miniType editRef),
                                       fromMMiSSVariantSpecToXml (
                                          linkVariants editRef
                                          )
                                       )
                                    )
                                 content0

                           elementWE 
                              <- fromEdit editFormatConverter name content1

                           element <- coerceWithErrorOrBreakIO break 
                              elementWE

                           modifyMVar_ preambleLinksMVar 
                              (\ preambleLinks ->
                                 -- see preambleLinksMVar code in 
                                 -- MMiSSReadObject.
                                 let
                                    callSite = case preambleLinks of
                                       [] -> Nothing
                                       _ -> Just entitySearchName

                                    extraData = MMiSSExtraPreambleData {
                                       callSite = callSite
                                       }
                                 in
                                    return (
                                       (preambleLink,extraData) 
                                          : preambleLinks
                                       )
                                 )

                           return (Just (element,(packageFolder1,nextMVar)))
               )                                  
         else
            return (hasValue Nothing)

-- | Function to be passed as second argument to MMiSSPackageFolder
doFile :: MVar ExportFiles -> MMiSSVariantSearch 
   -> (MMiSSPackageFolder,MVar [(Bool,EditRef)]) -> String -> IO ()
doFile mVar variantSearch0 (packageFolder0,_) file0 =
   modifyMVar_ mVar (return . ((packageFolder0,file0,variantSearch0) :))

---
-- Given an EditRef, extract a link to the referenced object.
getEditRef :: View -> EditRef -> IO (WithError (Link MMiSSObject))
getEditRef view editRef =
   lookupMMiSSObjectMustExist view (package editRef) (searchName editRef)

-- ----------------------------------------------------------------------
-- Other utility functions
-- ----------------------------------------------------------------------

---
-- Return variants corresponding to an EditRef
toVariants :: EditRef -> MMiSSVariantSearch
toVariants editRef =
   refineVariantSearch (outerVariants editRef) (linkVariants editRef)
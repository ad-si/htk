-- | This module contains the function for constructing the EmacsFS
-- (Emacs Filing System) required by "EmacsEdit", and the
-- 'PrintAction' that "EmacsEdit" requires.
module MMiSS.EmacsEdit(
   editMMiSSObjectXml,
      -- :: View -> Link MMiSSObject -> IO ()
      -- Edit the object as Xml
   editMMiSSObjectLaTeX,
      -- :: View -> Link MMiSSObject -> IO ()
      -- Edit the object as LaTeX

   ) where

import Data.Maybe

import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Exception(assert)

import Util.Computation
import Util.Thread
import Util.ExtendedPrelude
import Util.AtomString(toString,fromStringWE)
import Util.ReferenceCount
import Util.Messages
import Util.Broadcaster(broadcast)

import HTk.Toolkit.SimpleForm

import Types.ViewType
import Types.Link
import Imports.EntityNames
import Types.LinkManager

import Emacs.Content
import Emacs.Edit

import Text.XML.HaXml.Types

import MMiSS.LaTeX.Parser(IncludeInfo(..))

import MMiSS.BundleConvert
import MMiSS.ImportExportErrors
import MMiSS.ElementInfo
import MMiSS.BundleWrite
import MMiSS.BundleDissect
import MMiSS.DTDAssumptions
import MMiSS.Format
import MMiSS.EditFormatConverter
import MMiSS.ObjectTypeType
import MMiSS.ObjectType
import MMiSS.Variant
import MMiSS.EditXml(TypedName)
import MMiSS.LaTeX
import MMiSS.ObjectTypeInstance
import MMiSS.ReAssemble
import MMiSS.PackageFolder
import MMiSS.VariantObject
import MMiSS.EditLocks
import {-# SOURCE #-} MMiSS.ExportFiles


-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

-- | EditRef is the \"ref\" type passed to EmacsEdit.
--
-- The extra fields (apart from the link and variants) mean we can get what\'s
-- needed to do the Emacs buttons, on the basis of what\'s in the referencing
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
               errorMess mess
               done
         Right (package,fullName) ->
            do
               let
                  searchName :: EntitySearchName
                  searchName = FromAbsolute fullName

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

-- | This function constructs the file-system itself.
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

                  -- this function is put in to catch some cases where we
                  -- are refused read acccess to an object.
                  readLink' view link =
                     do
                        objectWE <- catchAllErrorsWE (readLink view link)
                        coerceWithErrorOrBreakIO break objectWE

               -- Get the object data
               objectLinkWE <- getEditRef view editRef
               objectLink <- coerceWithErrorOrBreakIO break objectLinkWE



               -- retrieve the object data.
               object <- readLink' view objectLink
               cacheSpecOpt <- lookupVariantObjectCacheWithSpec
                  (variantObject object) variants

               (cache,variantSpec) <- case cacheSpecOpt of
                  Nothing -> break ("No matching variant found for " ++ name)
                  Just cacheSpec -> return cacheSpec

               let
                  thisElement = cacheElement cache
                  thisLinkedObject = linkedObject object
                  thisObjectType = mmissObjectType object

               if getObjectTypeMiniType thisObjectType == miniType0
                  then
                     done
                  else
                     break ("Object "++name++" has wrong type")

               lockedWE <- acquireLock view (object,variantSpec)
               (releaseAct,lockSet) <- coerceWithErrorOrBreakIO break lockedWE

               let
                  -- redefine break so that it releases the lock, if something
                  -- goes wrong.  We use a horrible trick to do this when the
                  -- break is evaluated.
                  break2 mess = seq (unsafePerformIO releaseAct) (break mess)


               -- Extract a parent MMiSSPackageFolder and name for the object,
               -- to be used partly for looking up includes within the object,
               -- and also to provide a static way of writing back to the
               -- object.
               packageAndName1WE
                  <- getMMiSSPackageFolderAndName view thisLinkedObject

               (package1,name1)
                  <- coerceWithErrorOrBreakIO break2 packageAndName1WE

               -- Create variants used for searching in this object,
               -- We refine the existing variants with the one in the
               -- object's spec.
               let
                  -- outerVariants1 will become the outerVariants for contained
                  -- links.
                  outerVariants1 = refineVariantSearch variants variantSpec

                  (contentWE :: WithError (EmacsContent (TypedName,
                     IncludeInfo))) = toEdit name thisElement

               (content0 :: EmacsContent (TypedName,IncludeInfo))
                  <- coerceWithErrorOrBreakIO break2 contentWE

               let
                  mapContent :: EmacsContent (TypedName,IncludeInfo)
                     -> IO (EmacsContent EditRef)
                  mapContent = mapMonadic
                     (\ ((string,miniType),includeInfo) ->
                        do
                           (searchName1 :: EntitySearchName)
                              <- coerceWithErrorOrBreakIO break2
                                 (fromStringWE string)

                           let
                              includeAttributes = otherAttributes includeInfo

                              priorityWE = getPriorityAtt includeAttributes
                           priority <- coerceWithErrorOrBreakIO break2
                              priorityWE

                           let
                              linkVariants1 = case variantOpt includeInfo of
                                 Nothing -> emptyMMiSSVariantSpec
                                 Just variantElement ->
                                    toMMiSSVariantSpecFromXml variantElement

                              editRef =
                                 EditRef {
                                    package = package1,
                                    searchName = searchName1,
                                    outerVariants = outerVariants1,
                                    linkVariants = linkVariants1,
                                    miniType = miniType,
                                    priority = priority
                                    }

                           return editRef
                        )

               -- convert content0 into EmacsContent EditRef.
               content1 <- mapContent content0

               -- We now have to set up the EditedFile stuff
               let
                  writeData (emacsContent0 :: EmacsContent EditRef) =
                     catchAllErrorsWE (
                       do
                          let

                             (emacsContent1 :: EmacsContent (TypedName,
                                   IncludeInfo)) =
                                fmap
                                   (\ editRef ->
                                      ((toString (searchName editRef),
                                         miniType editRef),
                                         mkIncludeInfo editRef
                                         )
                                      )
                                   emacsContent0

                          elementWE <- fromEdit name emacsContent1
                          element0 <- coerceImportExportIO  elementWE

                          let
                             elInfo = ElementInfo {
                                packageIdOpt = Nothing,
                                packagePathOpt1 = Just name1,
                                packageNameOpt = Nothing,
                                labelOpt
                                   = Just (FromHere name1),
                                variants = linkVariants editRef
                                }


                          (bundle,packageId) <- parseBundle2 elInfo element0 []
                          writeBundle1 bundle (Just packageId) Nothing view
                             lockSet (Left (toLinkedObject package1))

                          emacsContentOpt <- case reduceElement element0 of
                             Nothing -> return Nothing
                             Just element1 ->
                                let
                                   contentWE :: WithError
                                      (EmacsContent (TypedName,IncludeInfo))
                                   contentWE =
                                      do
                                         element2
                                            <- changeLabel element1 searchName0
                                         toEdit name element2
                                in
                                   case fromWithError contentWE of
                                      Left mess ->
                                         do
                                            warningMess
                                               ("Commit of " ++ name
                                                  ++ " successful, but "
                                                  ++ "attempt to recompute "
                                                  ++ "magic buttons "
                                                  ++ "failed:\n"
                                                  ++ mess)
                                            return Nothing
                                      Right content0 ->
                                         do
                                            content1 <- mapContent content0
                                            return (Just content1)

                          messageMess ("Commit of "++name++ " successful!")
                          return emacsContentOpt
                       )

                  finishEdit =
                     do
                        releaseAct
                        remEdit object

                  (editedFile :: EditedFile EditRef) = EditedFile {
                     writeData = writeData,
                     finishEdit = finishEdit
                     }

               addEdit object

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


                  linkDetailsForm :: Form (EntitySearchName,(Char,String))
                  linkDetailsForm = form1 // (form3 // form5)

               linkDetailsOpt <- doForm "Enter Link Details" linkDetailsForm
               case linkDetailsOpt of
                  Nothing -> return Nothing
                  Just (searchName1,(miniType1,priority1)) ->
                     do
                        linkVariantSpecOpt <- editMMiSSVariantSpec
                           emptyMMiSSVariantSpec
                        case linkVariantSpecOpt of
                           Nothing -> return Nothing
                           Just linkVariantSpec ->
                              let
                                 editRef = EditRef {
                                    package = package1,
                                    searchName = searchName1,
                                    outerVariants = outerVariants1,
                                    linkVariants = linkVariantSpec,
                                    priority = priority1,
                                    miniType = miniType1
                                    }
                              in
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

-- | Returns the miniType for an object.
getObjectMiniType :: MMiSSObject -> Char
getObjectMiniType object = getObjectTypeMiniType (mmissObjectType object)

-- | Returns the miniType for an object type.
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
      broadcast (isEditedBroadcaster object) True

remEdit :: MMiSSObject -> IO ()
remEdit object =
   do
      doUnset <- remRef (editCount object)
      if doUnset
         then
            broadcast (isEditedBroadcaster object) False
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
            -- To gather all the package folders we put them in this MVar.
            -- (As MMiSSReadObject does in a similar situation)
            (packageFoldersMVar :: MVar [MMiSSPackageFolder])
               <- newMVar []


            -- To gather the export files we put them in this MVar
            (exportFilesMVar :: MVar ExportFiles)
               <- newMVar []

            topMVar <- newMVar [(True,topRef)]


            let
               variantSearch = refineVariantSearch (outerVariants topRef)
                  (linkVariants topRef)

            elementWE <- reAssemble
               (reAssembleArg view packageFoldersMVar getContent
                  editFormatConverter)
               (doFile exportFilesMVar)
               (searchName topRef) variantSearch
               (package topRef,topMVar)

            case fromWithError elementWE of
               Left error -> errorMess error
               Right element ->
                  do
                     -- Extract all preambles and exportFiles
                     packageFolders1 <- takeMVar packageFoldersMVar
                     let
                        packageFolders =
                           uniqOrdByKey
                              toLinkedObject
                              packageFolders1


                     exportFiles <- takeMVar exportFilesMVar

                     -- We do the actual printing in a separate thread,
                     -- so the user can continue editing.
                     forkIODebug (
                        do
                           stringWE <- exportElement view LaTeX
                              packageFolders element
                           case fromWithError stringWE of
                              Left error -> errorMess error
                              Right str ->
                                 let
                                    nameOpt :: Maybe String
                                    nameOpt =
                                       do
                                          (_,nameOpt) <- searchNameDirBase (
                                             searchName topRef)
                                          name <- nameOpt
                                          return (toString name)
                                 in
                                     mmissLaTeX view
                                        (fromMaybe "BogusFile" nameOpt)
                                        str exportFiles
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
reAssembleArg :: View -> MVar [MMiSSPackageFolder]
   -> (EditRef -> IO (WithError (EmacsContent (Bool,EditRef))))
   -> EditFormatConverter
   -> EntitySearchName
   -> MMiSSVariantSearch -> (MMiSSPackageFolder,MVar [(Bool,EditRef)])
   -> IO (WithError (Maybe (Element,
      (MMiSSPackageFolder,MVar [(Bool,EditRef)]))))
reAssembleArg view packageFoldersMVar getContent editFormatConverter
      entitySearchName variantSearch0 (packageFolder0,mVar) =
   do
      ((doExpand,editRef):rest) <- takeMVar mVar

      putMVar mVar rest
      assert (entitySearchName == searchName editRef) done

      let
         name = toString (searchName editRef)

         packageAct :: IO (WithError MMiSSPackageFolder)
         packageAct =
            do
               objectLinkWE <- getEditRef view editRef
               mapWithErrorIO'
                  (\ objectLink ->
                     do
                        mmissObject
                           <- readLink view objectLink
                        getMMiSSPackageFolder view mmissObject
                     )
                  objectLinkWE

      if doExpand
         then
            addFallOutWE (\ break ->
               do
                  -- Get the preamble link first of all,
                  -- as if we can't get the preamble we
                  -- can't print the object.
                  packageFolderWE <- packageAct
                  case fromWithError packageFolderWE of
                     Left mess ->
                        do
                           errorMess (name ++ ": " ++ mess)
                           return Nothing
                     Right packageFolder1 ->
                        do
                           (content0WE
                                 :: WithError (EmacsContent (Bool,EditRef)))
                              <- getContent editRef

                           content0 <- coerceWithErrorOrBreakIO break
                              content0WE

                           nextMVar <- newMVar (toEmacsLinks content0)

                           let
                              content1 :: EmacsContent (TypedName,IncludeInfo)
                              content1 = fmap
                                 (\ (b,editRef) ->
                                    ((toString (searchName editRef),
                                          miniType editRef),
                                       mkIncludeInfo editRef
                                       )
                                    )
                                 content0

                           elementWE
                              <- fromEdit editFormatConverter name content1

                           element <- coerceWithErrorOrBreakIO break
                              elementWE


                           modifyMVar_ packageFoldersMVar
                              (\ packageFolders
                                 -> return (packageFolder1 : packageFolders)
                                 )

                           return (Just (element,(packageFolder1,nextMVar)))
               )
         else
            return (hasValue Nothing)

-- | Function to be passed as second argument to reAssemble
doFile :: MVar ExportFiles -> MMiSSVariantSearch
   -> (MMiSSPackageFolder,MVar [(Bool,EditRef)]) -> EntityFullName -> IO ()
doFile mVar variantSearch0 (packageFolder0,_) file0 =
   modifyMVar_ mVar (return . ((packageFolder0,file0,variantSearch0) :))

-- | Given an EditRef, extract a link to the referenced object.
getEditRef :: View -> EditRef -> IO (WithError (Link MMiSSObject))
getEditRef view editRef =
   lookupMMiSSObjectMustExist view (package editRef) (searchName editRef)

-- ----------------------------------------------------------------------
-- Other utility functions
-- ----------------------------------------------------------------------

-- | Return variants corresponding to an EditRef
toVariants :: EditRef -> MMiSSVariantSearch
toVariants editRef =
   refineVariantSearch (outerVariants editRef) (linkVariants editRef)


mkIncludeInfo :: EditRef -> IncludeInfo
mkIncludeInfo editRef =
   let
      variants1 = linkVariants editRef

      variantOpt =
         if variants1 == emptyMMiSSVariantSpec
            then
               Nothing
            else
               Just (fromMMiSSVariantSpecToXml variants1)

      otherAttributes = setPriorityAtt [] (priority editRef)
   in
      IncludeInfo {
         variantOpt = variantOpt,
         otherAttributes = otherAttributes
         }

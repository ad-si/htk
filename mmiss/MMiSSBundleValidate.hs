-- | This module contains the functions for validating a bundle.
-- 
-- The following things are checked.  For commentting purposes
-- we also give a name to each check, for example DISTINCT.
-- DISTINCT) PackageId's in a bundle, names (as given by namefileLoc) in a 
--    folder, variants in an object are all distinct. 
-- DIROBJECT) folders and package folders contain BundleNodeData's with Dir,
--    other sorts have Object.
-- TOPSDIRS) All the top-level bundle-nodes have BundleNodeData with Dir.
-- PREAMBLE1) Each package folder contains at most one preamble.
-- STRUCTURE) package folders contain just preambles, MMiSS objects, MMiSS 
--    files and mmiss sub folders.  MMiSS sub folders contain just MMiSS 
--    objects, MMiSS files and MMiSS sub folders
-- 
--    MMiSS sub folders, MMiSS objects, MMiSS files and preambles do not occur
--    except in MMiSS package folders and MMiSS sub folders.
-- VARIANTS) Plain files and preambles do not specify any variants; MMiSS 
--    objects and MMiSS files always do.
-- ENTITYNAME) The name constructed from the FileLoc (with nameFileLoc) 
--    should always be a valid EntityName.
-- NAMES) Everything except preambles or top-level nodes have 
--    name = Just [something] in their fileLoc.  Preambles don't.
-- EXT) ext is only set for MMiSS files.  extra is only set for ordinary files
--    and folders.
-- EXTEXISTS) for MMiSS files, ext is a file type known to MMiSSFileType.
-- UNKNOWN) The bundle contains no UnknownType, NoData or NoText elements.
-- DTD) Elements pass the DTD. 
-- LINKNAMES) Names in links are valid EntityFullNames.
-- FILENAMES) Files referred to are valid EntityFullNames.
-- NONEMPTYOBJECTS) Every Object item contains at least one element.
-- CONSISTENTTAGS) The tags in MMiSS objects should all be the same.
-- NOUNICODE) No BundleTexts contain a CharType of Unicode.
--    (This is currently only useful for MMiSSObjects, and for them the
--    UTF8 will have been converted to an Element anyway.)
module MMiSSBundleValidate(
   validateBundle, -- :: Bundle -> WithError ()
   
   validateBundleOut, -- :: Bundle -> IO ()
      -- used, at least during debugging, for validating bundles before they
      -- are transmitted to the user.
   ) where

import List
import Monad
import Maybe

import Text.XML.HaXml.Types
import XmlExtras

import Messages
import Computation
import ExtendedPrelude
import AtomString

import EntityNames

import MMiSSBundle
import MMiSSBundleTypes
import MMiSSBundleSimpleUtils
import MMiSSDTD
import MMiSSDTDAssumptions
import MMiSSVariant
import MMiSSElementInstances
import MMiSSFileType

-- | Check that a bundle being read in passes various structural
-- requirements
validateBundle :: Bundle -> WithError ()
validateBundle bundle =
   do
      validateBundle0 bundle -- UNKNOWN NONEMPTYOBJECTS
      validateBundle1 bundle -- DIROBJECT STRUCTURE
      validateBundle2 bundle -- NAMES EXT EXTEXISTS
      validateBundle3 bundle -- TOPDIRS
      validateBundle4 bundle -- VARIANTS
      validateBundle5 bundle -- DISTINCT, PREAMBLE1 & ENTITYNAME
      validateBundle6 bundle -- DTD, LINKNAMES, FILENAMES & CONSISTENTTAGS
      validateBundle7 bundle -- NOUNICODE


-- | Check that a bundle being output passes various structural
-- requirements.  These are less strict than for validateBundle because,
-- for example, text fields might not be filled in, if the user only asked
-- for locations.
validateBundleOut :: Bundle -> IO ()
validateBundleOut bundle =
   case fromWithError (validateBundleOut1 bundle) of
      Right () -> done
      Left mess ->
         alertMess ("Exported Bundle fails validation check: \n" ++ mess)

validateBundleOut1 :: Bundle -> WithError ()
validateBundleOut1 bundle =
   do
      validateBundle1 bundle -- DIROBJECT STRUCTURE
      validateBundle2 bundle -- NAMES EXT EXTEXISTS
      validateBundle3 bundle -- TOPDIRS
      validateBundle4 bundle -- VARIANTS
      validateBundle5 bundle -- DISTINCT, PREAMBLE1 & ENTITYNAME
      validateBundle6 bundle -- DTD, LINKNAMES, FILENAMES & CONSISTENTTAGS


-- -----------------------------------------------------------------------
-- UNKNOWN & NONEMPTYOBJECTS
-- -----------------------------------------------------------------------

validateBundle0 :: Bundle -> WithError ()
validateBundle0 = checkAllNodes checkUnknown
   where
      checkUnknown :: BundleNode -> WithError ()
      checkUnknown bundleNode1 =
         case (base . objectType . fileLoc $ bundleNode1,
               bundleNodeData bundleNode1) of
            (UnknownType,_) -> err bundleNode1
               "unknown type"
            (_,NoData) -> err bundleNode1 "no data"
            (_,Object []) -> err bundleNode1 "No variants supplied"
            (_,Object objects) ->
               checkList
                  (\ (_,text) -> case text of
                     NoText -> err bundleNode1 "variant has no text"
                     _ -> done
                     )
                  objects
            (_,Dir _) -> done
      
-- -----------------------------------------------------------------------
-- DIROBJECT, STRUCTURE.  Other checks in this folder may assume
-- STRUCTURE has been carried out. 
-- -----------------------------------------------------------------------

validateBundle1 :: Bundle -> WithError ()
validateBundle1 = checkAllNodes checkStructure
   where
      checkStructure bundleNode0 =
         case bundleNodeData bundleNode0 of
            Object _ -> 
               if containerType0 == NotContainer
                  then
                     done
                  else
                     err bundleNode0 " a folder, but has object information"
            Dir bundleNodes ->
               do
                  if containerType0 == NotContainer 
                     then
                        err bundleNode0 " not a folder, but contains objects"
                     else
                        done

                  checkList checkContain bundleNodes
            NoData -> done
         where
            containerType0 = toContainerType bundleNode0

            checkContain bundleNode1 = 
               if mayContain bundleNode1 
                  then
                     done
                  else
                     err bundleNode0 (" should not contain " 
                         ++ describeFileLoc (fileLoc bundleNode1))

            mayContain bundleNode1 = case (containerType0,
                  base . objectType . fileLoc $ bundleNode1,
                  toContainerType bundleNode1) of
               (Folder,_,Folder) -> True
               (Folder,_,PackageFolder) -> True
               (Folder,FileEnum,_) -> True
               (PackageFolder,_,SubFolder) -> True
               (PackageFolder,MMiSSObjectEnum,_) -> True
               (PackageFolder,MMiSSFileEnum,_) -> True
               (PackageFolder,MMiSSPreambleEnum,_) -> True
               (SubFolder,_,SubFolder) -> True
               (SubFolder,MMiSSObjectEnum,_) -> True
               (SubFolder,MMiSSFileEnum,_) -> True
               _ -> False
            
data ContainerType = Folder | SubFolder | PackageFolder | NotContainer
   deriving (Eq)

toContainerType :: BundleNode -> ContainerType
toContainerType bundleNode1 =
   case objectType (fileLoc bundleNode1) of
      BundleType {base = FolderEnum,extra = extra1} ->
         if extra1 == extra mmissSubFolderType
            then
               SubFolder
            else
               Folder
      BundleType {base = MMiSSFolderEnum} -> PackageFolder
      _ -> NotContainer

-- -----------------------------------------------------------------------
-- NAMES EXT EXTEXISTS
-- -----------------------------------------------------------------------

validateBundle2 :: Bundle -> WithError ()
validateBundle2 = checkAllNodes1 checkNamesExt
   where
      checkNamesExt isTop bundleNode1 =
         let
            fileLoc1 = fileLoc bundleNode1

            objectType1 = objectType fileLoc1

            mustHaveName =
               case (name fileLoc1,isTop,base objectType1) of
                  (Just _,_,_) -> done
                  (_,True,MMiSSFolderEnum) -> done
                  (_,True,FolderEnum) -> done
                  _  -> err bundleNode1 "Object has no name"
            mustNotHaveName =
               case name fileLoc1 of
                  Just _ -> err bundleNode1 "Preamble has a name"
                  Nothing -> done

            mustHaveFileExt =
               case ext objectType1 of
                  Nothing -> err bundleNode1 "Type specifies no extension"
                  Just "tex" -> done -- "tex" and "xml" occur in exported
                  Just "xml" -> done -- bundles for MMiSS objects.
                  Just ext1 -> if fileTypeExists ext1
                     then
                        done
                     else
                        err bundleNode1 ("File type " ++ ext1 ++
                           " is not known")
            mustNotHaveExt =
               case ext objectType1 of
                  Just _ -> err bundleNode1 "Type specifies an extension"
                  Nothing -> done

            mustHaveExtra =
               case extra objectType1 of
                  Nothing -> err bundleNode1 "Type specifies no extra data"
                  Just _ -> done
            mustNotHaveExtra =
               case extra objectType1 of
                  Just _ -> err bundleNode1 "Type specifies extra data"
                  Nothing -> done

         in
            case base objectType1 of
               FolderEnum -> mustHaveName >> mustNotHaveExt >> mustHaveExtra
               FileEnum -> mustHaveName >> mustNotHaveExt >> mustHaveExtra
               MMiSSFolderEnum -> 
                  mustHaveName >> mustNotHaveExt >> mustNotHaveExtra
               MMiSSObjectEnum ->
                  mustHaveName >> mustNotHaveExt >> mustNotHaveExtra
               MMiSSFileEnum -> 
                  mustHaveName >> mustHaveFileExt >> mustNotHaveExtra
               MMiSSPreambleEnum -> 
                  mustNotHaveName >> mustNotHaveExt >> mustNotHaveExtra
               UnknownType -> done 

-- -----------------------------------------------------------------------
-- TOPDIRS
-- -----------------------------------------------------------------------

validateBundle3 :: Bundle -> WithError ()
validateBundle3 (Bundle packageBundles) =
   checkList 
      (\ (_,bundleNode) -> case bundleNodeData bundleNode of
         Dir _ -> done
         _ -> err bundleNode "is top-level in bundle but not a directory"
         )
      packageBundles
               
-- -----------------------------------------------------------------------
-- VARIANTS
-- -----------------------------------------------------------------------

validateBundle4 :: Bundle -> WithError ()
validateBundle4 = checkAllNodes checkVariants
   where
      checkVariants bundleNode1 =
         case base . objectType . fileLoc $ bundleNode1 of
            MMiSSObjectEnum -> mustHaveVariants
            MMiSSFileEnum -> mustHaveVariants
            MMiSSPreambleEnum -> mustNotHaveVariants
            FileEnum -> mustNotHaveVariants
            _ -> done

         where
            variants0 = toVariants bundleNode1

            mustHaveVariants = checkList
               (\ (variantSpecOpt,_) -> case variantSpecOpt of
                  Nothing -> err bundleNode1 
                     "Object text does not specify any variants"
                     -- In fact I think this should never happen.
                  Just _ -> done
                  )
               variants0
            mustNotHaveVariants = checkList
               (\ (variantSpecOpt,_) -> case variantSpecOpt of
                  Just _ -> err bundleNode1 
                     "Object specifies illegal variants"
                  Nothing -> done
                  )
               variants0

-- -----------------------------------------------------------------------
-- DISTINCT, PREAMBLE1 & ENTITYNAME (assumes NAMES)
-- -----------------------------------------------------------------------

validateBundle5 :: Bundle -> WithError ()
validateBundle5 (bundle @ (Bundle packageBundles)) =
   do
      case findDuplicate fst packageBundles of
         Just (packageId,_) ->
            fail ("PackageId " ++ toString packageId 
               ++ " occurs multiple times")
         Nothing -> done
      checkAllNodes checkDistinct bundle
   where
      checkDistinct bundleNode1 = case bundleNodeData bundleNode1 of
         Object variants0 ->
            case findDuplicate fst variants0 of
               Just (variantOpt,_) ->
                  err bundleNode1 
                     (describeVariants variantOpt ++ " occurs multiple times")
               Nothing -> done
         Dir bundleNodes0 ->
            do
               let
                 (preambles,bundleNodes1) = partition
                    (\ bundleNode2 
                       -> (base . objectType . fileLoc $ bundleNode2) 
                          == MMiSSPreambleEnum
                       )
                    bundleNodes0
               case (preambles,base . objectType . fileLoc $ bundleNode1) of
                  ([preamble],MMiSSFolderEnum) -> done
                  ([],_) -> done
                  _ -> err bundleNode1 "Folder has too many preambles"
               bundleNames <- 
                    -- everything not a Preamble should have a name, assuming
                    -- NAMES.
                  mapM
                     (\ bundleNode2 -> 
                        do
                           let
                              fileLoc1 = fileLoc bundleNode2 
                              nameWE = nameFileLoc fileLoc1

                           case fromWithError nameWE of
                              Left _ -> err bundleNode1 
                                 (" subobject " ++ describeFileLoc fileLoc1
                                    ++ " does not have a valid name")
                              Right bundleName -> return bundleName
                         )
                     bundleNodes1
               case findDuplicate id bundleNames of
                  Nothing -> done
                  Just name -> err bundleNode1 
                     ("contains multiple elements called " ++ show name)
         NoData -> done      
                  
-- -----------------------------------------------------------------------
-- DTD, LINKNAMES, FILENAMES & CONSISTENTTAGS
-- -----------------------------------------------------------------------

validateBundle6 :: Bundle -> WithError ()
validateBundle6 = checkAllNodes checkDTD
   where
      checkDTD bundleNode1 =
         case base . objectType . fileLoc $ bundleNode1 of
            MMiSSObjectEnum ->
               let
                  textsToCheck = toVariants bundleNode1
               in
                  do
                     checkList
                        (\ (variantSpecOpt,bundleText) ->
                           case bundleText of
                              NoText -> done
                              _ -> 
                                 do
                                    element <- fromBundleTextWE bundleText
                                    case validateElement0 element of
                                       [] -> done
                                       errors ->
                                          err bundleNode1
                                             (  describeVariants 
                                                   variantSpecOpt
                                                ++ " has errors:\n"
                                                ++ unlines errors
                                                )
                                    let
                                       links :: [(LinkType,String)]
                                       links = mapMaybe 
                                          classifyLink
                                          (getAllElements1 element)

                                    checkList
                                       (\ (_,linkName) ->
                                          case fromWithError 
                                                (fromStringWE linkName) of
                                             Right (_ :: EntitySearchName) -> 
                                                done
                                             Left mess ->
                                                err bundleNode1
                                                   (linkName ++ 
                                                      " is not a proper"
                                                      ++ " link name")
                                          )
                                       links

                                    let
                                       files :: [String]
                                       files = getFiles element
                                          -- Yes this does get all files
                                          -- from all sub-elements (not just
                                          -- the head element)

                                    checkList
                                       (\ file -> 
                                          case fromWithError 
                                                (fromStringWE file) of
                                             Right (_ :: EntityFullName) ->
                                                done
                                             Left mess ->
                                                err bundleNode1
                                                   (file ++ 
                                                      " is not a proper"
                                                      ++ " file name")
                                          )
                                       files
                                   
                        )
                        textsToCheck

                     case fromWithError (getTag bundleNode1) of
                        Left mess -> err bundleNode1 mess
                        Right _ -> done
            _ -> done        


-- -----------------------------------------------------------------------
-- NOUNICODE
-- -----------------------------------------------------------------------

validateBundle7 :: Bundle -> WithError ()
validateBundle7 = checkAllNodes checkNoUnicode
   where
      checkNoUnicode bundleNode1 =
         checkList
            (\ (variantOpt,text) ->
               case text of
                  BundleString {charType = Unicode} ->
                     err bundleNode1 
                        (describeVariants variantOpt
                           ++ " has Unicode")
                  _ -> done
               )
            (toVariants bundleNode1)
         
-- -----------------------------------------------------------------------
-- Tools for constructing checking functions
-- -----------------------------------------------------------------------

checkAllNodes :: (BundleNode -> WithError ()) -> Bundle -> WithError ()
checkAllNodes checkBundleNode (Bundle packageBundles) =
   let
      cNodes :: [BundleNode] -> WithError ()
      cNodes nodes = checkList cNode nodes

      cNode :: BundleNode -> WithError ()
      cNode bundleNode =
         do
            checkBundleNode bundleNode
            case bundleNodeData bundleNode of
               Dir bundleNodes -> cNodes bundleNodes
               _ -> done
   in
      cNodes (map snd packageBundles)


-- checkAllNodes1 provides an extra argument which indicates if this
-- is the head node.
checkAllNodes1 :: (Bool -> BundleNode -> WithError ()) -> Bundle 
   -> WithError ()
checkAllNodes1 checkBundleNode (Bundle packageBundles) =
   let
      cNodes :: Bool -> [BundleNode] -> WithError ()
      cNodes isTop nodes = checkList (cNode isTop) nodes

      cNode :: Bool -> BundleNode -> WithError ()
      cNode isTop bundleNode =
         do
            checkBundleNode isTop bundleNode
            case bundleNodeData bundleNode of
               Dir bundleNodes -> cNodes False bundleNodes
               _ -> done
   in
      cNodes True (map snd packageBundles)
   



-- slightly better than mapM_ because if it discovers multiple errors
-- it returns them all.
checkList :: (a -> WithError ()) -> [a] -> WithError ()
checkList checkA as =
   do
      let
         unitListWE = checkList1 checkA as
      unitList <- unitListWE
      done

checkList1 :: (a -> WithError b) -> [a] -> WithError [b]
checkList1 checkA as = listWithError (map checkA as)


-- -----------------------------------------------------------------------
-- The error function
-- -----------------------------------------------------------------------

describeVariants :: Maybe MMiSSVariantSpec -> String
describeVariants variantOpt =
   case variantOpt of
      Nothing -> "Text"
      Just variantSpec ->
         if variantSpec == emptyMMiSSVariantSpec
            then
               "Variant with no attributes"
            else
               "Variant " ++ show variantSpec

err :: BundleNode -> String -> WithError a
err node str = fail ("Error for " ++ describeFileLoc (fileLoc node) 
   ++ ": " ++ str)
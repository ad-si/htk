-- | This module contains the functions for checking that the
-- bundle types within a bundle are compatible with what's already there.
module MMiSSBundleNodeCheckTypes(
   checkBundleNodeTypes,
   ) where

import Computation
import AtomString

import View
import Files
import Folders
import LinkManager
import ObjectTypes
import AttributesType

import MMiSSInsertionPoint
import MMiSSBundle
import MMiSSSplitLink
import MMiSSObjectTypeType
import MMiSSFileType
import MMiSSBundleSimpleUtils

-- -------------------------------------------------------------------------
-- checkBundleNodeTypes
-- -------------------------------------------------------------------------

checkBundleNodeTypes 
   :: View -> InsertionPoint -> BundleNode -> IO (WithError ())
checkBundleNodeTypes view insertionPoint bundleNode =
   do
      check1WE <-
         case insertionPoint of
            Left linkedObject -> 
               checkBundleNodeTypes1 view (Just linkedObject) bundleNode
            Right _ ->
               return done

      check2WE <- checkSimpleTypesExist view bundleNode
      let
         result =
            do
               checks <- pairWithError check1WE check2WE
               done
      return result
                        
checkBundleNodeTypes1 
   :: View -> Maybe LinkedObject -> BundleNode -> IO (WithError ())
checkBundleNodeTypes1 _ Nothing _ = return (hasValue ())
checkBundleNodeTypes1 view (Just linkedObject) bundleNode =
   do
      let
         wrappedLink = toWrappedLink linkedObject

      wrappedObject <- wrapReadLink view wrappedLink
      let
         wrappedObjectType = getObjectType wrappedObject

         typesMatch =
            case (splitWrappedLink wrappedLink,
                  toString (objectTypeId wrappedObjectType),
                  base . objectType . fileLoc $ bundleNode,
                  extra . objectType . fileLoc $ bundleNode) of

               -- don't bother with preamble since it can't happen
               (FileC _,fileExtra0,FileEnum,fileExtra1Opt) 
                  | Just fileExtra0 == fileExtra1Opt
                  -> True
               (FolderC _,folderExtra0,FolderEnum,folderExtra1Opt) 
                  | Just folderExtra0 == folderExtra1Opt
                  -> True
               (MMiSSPackageFolderC _,_,MMiSSFolderEnum,_)
                  -> True
                  -- we don't bother to check extra fields where they are
                  -- redundant.  (validateBundle does that.)
               (MMiSSObjectC _,xmlTagKeyStr,MMiSSObjectEnum,_) ->
                  let
                     xmlTag = coerceWithError (getTag bundleNode)
                     xmlTagKey2 = MMiSSObjectTypeType.constructKey xmlTag
                     xmlTagKeyStr2 = toString xmlTagKey2
                  in
                     xmlTagKeyStr == xmlTagKeyStr2
               (MMiSSFileC _,extKeyStr,MMiSSFileEnum,_) ->
                  let
                     Just ext2 = ext . objectType . fileLoc $ bundleNode
                     extKey2 = MMiSSFileType.constructKey ext2
                     extKeyStr2 = toString extKey2
                  in
                     extKeyStr == extKeyStr2 
               _ -> False

         subNodes = case bundleNodeData bundleNode of
            Dir nodes -> nodes
            _ -> []

         checkSubNode :: BundleNode -> IO (WithError ())
         checkSubNode bundleNode1 =
            case fromWithError (nameFileLocOpt (fileLoc bundleNode1)) of
               Left mess -> return (hasError mess)
               Right Nothing -> return (hasValue ())
               Right (Just name) ->
                  do
                     linkedObjectOpt <- lookupNameInFolder
                        linkedObject name
                     checkBundleNodeTypes1 view linkedObjectOpt bundleNode1

      if typesMatch 
         then
            do
               (nodeResults :: [WithError ()]) <- mapM checkSubNode subNodes
               let
                  finalResult =
                     do
                        unitList <- listWithError nodeResults
                        done
               return finalResult
         else
            return (hasError (
               "Type conflict in repository: unable to write node " 
               ++ describeFileLoc (fileLoc bundleNode)))

-- -------------------------------------------------------------------------
-- checkSimpleTypesExist
-- -------------------------------------------------------------------------

checkSimpleTypesExist :: View -> BundleNode -> IO (WithError ())
checkSimpleTypesExist view bundleNode =
   do
      let
         trialBundle = Bundle [(PackageId (error "bad packageid"),bundleNode)]
        
         allNodes :: [BundleNode]
         allNodes = map snd (getAllNodes trialBundle)

      (unitWEs :: [WithError ()]) <- mapM
         (\ node -> case base . objectType . fileLoc $ node of
            FileEnum ->
               do
                  let
                     Just extra1 = extra . objectType . fileLoc $ node

                  (fileTypeOpt :: Maybe FileType) <- getObjectTypeByKeyOpt 
                     view (fromString extra1) 
                  case fileTypeOpt of
                     Just fileType -> 
                        if isEmptyAttributesType (
                              Files.requiredAttributes fileType)
                           then
                              return (hasValue ())
                           else
                              return (hasError ("File type " ++ extra1
                                 ++ " has attributes, which we can't handle"
                                 ++ " in bundles yet"))
                     Nothing -> 
                        return (hasError ("File type " ++ extra1 
                           ++ " not found in this view"))
            FolderEnum ->
               do
                  let
                     Just extra1 = extra . objectType . fileLoc $ node

                  (folderTypeOpt :: Maybe FolderType) <- getObjectTypeByKeyOpt 
                     view (fromString extra1) 
                  case folderTypeOpt of
                     Just folderType -> 
                        if isEmptyAttributesType (
                              Folders.requiredAttributes folderType)
                           then
                              return (hasValue ())
                           else
                              return (hasError ("Folder type " ++ extra1
                                 ++ " has attributes, which we can't handle"
                                 ++ " in bundles yet"))
                     Nothing -> 
                        return (hasError ("Folder type " ++ extra1 
                           ++ " not found in this view"))
            _ -> return (hasValue ())
            )  
         allNodes
      let
         result =
            do
               units <- listWithError unitWEs
               done
      return result

-- -------------------------------------------------------------------------
-- Utilities
-- -------------------------------------------------------------------------


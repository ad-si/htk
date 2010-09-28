-- | The NoAccessObject is an object type used to replace objects the
-- user is not allowed to read.
module Types.NoAccessObject(
   NoAccessObject,NoAccessObjectType,
      -- Instances of ObjectType, HasLinkedObject
   createNoAccessObject,
      -- :: View -> WrappedLink -> IO LinkedObject
      -- Create a new NoAccessObject.  This will have a LinkedObject with
      -- no parent or name; these should be set if possible by the
      -- caller.
   isNoAccessLink,
      -- :: WrappedLink -> Bool
      -- Returns True if this link is for a NoAccessObject.
   registerNoAccessObjectType, -- :: IO ()
   noAccessObjectToLinkedObject,
      -- :: NoAccessObject -> LinkedObject

   ) where

import Maybe

import System.IO.Unsafe
import Data.Typeable
import Control.Monad.Fix

import Util.BinaryAll
import Util.AtomString
import Util.Computation
import Util.ExtendedPrelude
import Util.Sources
import Util.VariableSet (toKey)

import Graphs.GraphDisp
import Graphs.GraphConfigure

import Imports.EntityNames

import Types.CodedValue
import Types.ObjectTypes
import Types.Link
import Types.LinkManager
import Types.ViewType
import Types.MergeTypes
import Types.GlobalRegistry
import Types.DisplayParms
import Types.ViewType(View)
import Types.SpecialNodeActions(emptyNodeActions)

-- ----------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------

data NoAccessObjectType = NoAccessObjectType deriving (Typeable)

newtype NoAccessObject = NoAccessObject {
   linkedObject :: LinkedObject
   } deriving (Typeable)

-- -----------------------------------------------------------------------
-- Trivial instances
-- -----------------------------------------------------------------------

instance Monad m => HasBinary NoAccessObjectType m where
   readBin = error "NoAccessObject: attempt to read NoAccessObjectType"
   writeBin = error "NoAccessObject: attempt to write NoAccessObjectType"

instance Monad m => HasBinary NoAccessObject m where
   readBin = error "NoAccessObject: attempt to read NoAccessObject"
   writeBin = error "NoAccessObject: attempt to write NoAccessObject"

instance HasLinkedObject NoAccessObject where
   toLinkedObject = linkedObject

-- | We don't attempt merging at all.  We can't, because we would need
-- to read the object to reassign its locations.
instance HasMerging NoAccessObject where
   getMergeLinks = emptyMergeLinks

   attemptMerge _ _ _ ((view,_,noAccessObject):_) =
      do
         name <- getFullName view noAccessObject
         return (hasError (
            "Cannot complete merge, because no read access to "
            ++ name))

instance ObjectType NoAccessObjectType NoAccessObject where
   objectTypeTypeIdPrim _ = noAccessObjectTypeId
   objectTypeIdPrim _ = noAccessTypeKey
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim _ = NoAccessObjectType
   extraObjectTypes = return [NoAccessObjectType]
   createObjectTypeMenuItemPrim _ = Nothing
   createObjectMenuItemPrim _ = Nothing
   toLinkedObjectOpt noAccessObject = Just (toLinkedObject noAccessObject)
   nodeTitleSourcePrim noAccessObject =
      fmap
         (\ nameOpt -> fromMaybe "No Access Object" (fmap toString nameOpt))
         (getLinkedObjectTitleOpt (toLinkedObject noAccessObject))

   getNodeDisplayData view _ NoAccessObjectType _ =
      let
         (theNodeType :: NodeType) = fromString ""

         nodeDisplayData = NodeDisplayData {
            topLinks = [],
            arcTypes = [],
            nodeTypes = [(theNodeType,
               Color "purple" $$$
               valueTitleSource view $$$
               emptyNodeTypeParms
               )],
            getNodeType = const theNodeType,
            getNodeLinks = const (return emptyArcEnds),
            specialNodeActions = const emptyNodeActions
            }
      in
         return (Just nodeDisplayData)

-- -------------------------------------------------------------------
-- Creating and distinguishing
-- -------------------------------------------------------------------

createNoAccessObject :: View -> WrappedLink -> IO NoAccessObject
createNoAccessObject view (wrappedLink @ (WrappedLink link)) =
   Control.Monad.Fix.mfix
      (\ noAccessObject ->
         do
            link2 <- pokeLink view link noAccessObject
            let
               wrappedLink2 = WrappedLink link2

            insertionOpt <- getInsertion view wrappedLink

            linkedObject
               <- newLinkedObjectNoParent view wrappedLink2 insertionOpt

            return (
               NoAccessObject {
                  linkedObject = linkedObject
                  }
               )
         )

isNoAccessLink :: WrappedLink -> Bool
isNoAccessLink wrappedLink =
   linkObjectTypeTypeId wrappedLink == noAccessObjectTypeId

-- | to go into the .hi-boot file.
noAccessObjectToLinkedObject :: NoAccessObject -> LinkedObject
noAccessObjectToLinkedObject = linkedObject

noAccessObjectTypeId :: String
noAccessObjectTypeId = "NoAccessObject"


-- -------------------------------------------------------------------
-- Type hackery
--
-- We use some extremely unsavoury tricks for guessing the name
-- of a WrappedLink.  Basically
--    (1) we extract the parent location for link.
--    (2) we get the object dictionary's entry for that parent location and,
--        by scanning all available types, decode the Dyn into it to get the
--        actual parent object.
--    (3) we scan the parent object's contents, looking for a matching name.
-- -------------------------------------------------------------------

getInsertion :: View -> WrappedLink -> IO (Maybe Insertion)
getInsertion view wrappedLink1 =
   do
      parentLocationOpt <- getParentLocationInView view (toKey wrappedLink1)
      case parentLocationOpt of
         Nothing -> return Nothing
         Just parentLocation ->
            do
               parentWrappedLinkOpt
                  <- getWrappedLinkFromLocation view parentLocation
               case parentWrappedLinkOpt of
                  Nothing -> return Nothing
                  Just parentWrappedLink ->
                     do
                        (parentObject :: WrappedObject)
                           <- wrapReadLink view parentWrappedLink
                        let
                           parentLinkedObjectOpt
                              = wrapToLinkedObjectOpt parentObject
                        case parentLinkedObjectOpt of
                           Nothing -> return Nothing
                           Just parentLinkedObject ->
                              do
                                 (parentContents
                                       :: [(EntityName,WrappedLink)])
                                    <- readContents (
                                       listObjectContentsAsWrappedLinks
                                          parentLinkedObject)
                                 return (findJust
                                    (\ (name,wrappedLink2) ->
                                       if toKey wrappedLink1
                                             == toKey wrappedLink2
                                          then
                                             Just (mkInsertion
                                                parentLinkedObject name)
                                          else
                                             Nothing
                                       )
                                    parentContents
                                    )


-- -------------------------------------------------------------------
-- Registering
-- -------------------------------------------------------------------


registerNoAccessObjectType :: IO ()
registerNoAccessObjectType =
   registerObjectType (error "Unknown FolderType" :: NoAccessObjectType)

-- -------------------------------------------------------------------
-- The Global Registry.  This will in fact be empty.
-- -------------------------------------------------------------------

globalRegistry :: GlobalRegistry NoAccessObjectType
globalRegistry = unsafePerformIO mkGlobalRegistry
{-# NOINLINE globalRegistry #-}

noAccessTypeKey :: GlobalKey
noAccessTypeKey = oneOffKey "NoAccessObject" "NoAccessObject"

mkGlobalRegistry :: IO (GlobalRegistry NoAccessObjectType)
mkGlobalRegistry = createGlobalRegistry


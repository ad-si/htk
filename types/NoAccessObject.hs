-- | The NoAccessObject is an object type used to replace objects the
-- user is not allowed to read.
module NoAccessObject(
   NoAccessObject,NoAccessObjectType,
      -- Instances of ObjectType, HasLinkedObject
   createNoAccessObject,
      -- :: WrappedLink -> IO NoAccessObject
      -- Create a new NoAccessObject.  This will have a LinkedObject with
      -- no parent or name; these should be set if possible by the
      -- caller.
   registerNoAccessObjectType, -- :: IO ()
   ) where

import Maybe

import System.IO.Unsafe
import Data.Typeable

import BinaryAll 
import AtomString
import Computation

import GraphDisp
import GraphConfigure

import CodedValue

import ObjectTypes
import LinkManager
import MergeTypes
import GlobalRegistry
import DisplayParms
import ViewType(View)
import SpecialNodeActions(emptyNodeActions)

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
   objectTypeTypeIdPrim _ = "NoAccessObject"
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
-- Creating
-- -------------------------------------------------------------------

createNoAccessObject :: View -> WrappedLink -> IO NoAccessObject
createNoAccessObject view wrappedLink =
   do
      linkedObjectWE <- newLinkedObject view wrappedLink Nothing
      linkedObject <- coerceWithErrorIO linkedObjectWE
      let
         noAccessObject = NoAccessObject {
            linkedObject = linkedObject
            }

      return noAccessObject

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
   
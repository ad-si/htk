{- Files are simple files together with attributes

   This file is very similar to Folders.hs -}
module Files(
   registerFiles, -- :: IO ()
      -- to be done at initialisation
   File,
      -- instance of HasAttributes, HasFilePath
   FileType,
   newEmptyFile,
   ) where

import qualified IOExts(unsafePerformIO)

import Dynamics
import Computation
import AtomString
import Sink
import VariableSet
import UniqueString

import GraphDisp
import GraphConfigure
import Graph(ArcType,NodeType)

import View
import CodedValue
import Link
import BasicObjects
import AttributesType
import ObjectTypes
import DisplayParms
import GlobalRegistry

-- ------------------------------------------------------------------
-- FileType and its instance of HasCodedValue
-- ------------------------------------------------------------------

data FileType = FileType {
   fileTypeId :: AtomString,   
   requiredAttributes :: AttributesType,
   displayParms :: NodeTypes (String,Link File),
   knownFiles :: VariableSet (Link File)
   }

fileType_tyCon = mkTyCon "Files" "FileType"
instance HasTyCon FileType where
   tyCon _ = fileType_tyCon

instance HasCodedValue FileType where
   encodeIO = mapEncodeIO 
      (\ (FileType {fileTypeId = fileTypeId,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms})
         -> (Str fileTypeId,requiredAttributes,displayParms)
         )
   decodeIO codedValue0 view =
      do
         ((Str fileTypeId,requiredAttributes,displayParms),
            codedValue1) <- decodeIO codedValue0 view
         knownFiles <- newEmptyVariableSet
         return (FileType {fileTypeId = fileTypeId,
            requiredAttributes = requiredAttributes,
            displayParms = displayParms,
            knownFiles = knownFiles},codedValue1)


-- ------------------------------------------------------------------
-- File and its instance of HasAttributes and HasCodedValue
-- ------------------------------------------------------------------


data File = File {
   fileType :: FileType,
   attributes :: Attributes,
   name :: String,
   simpleFile :: SimpleFile
   }

file_tyCon = mkTyCon "Files" "File"
instance HasTyCon File where
   tyCon _ = file_tyCon

instance HasAttributes File where
   readPrimAttributes object = attributes object

instance HasCodedValue File where
   encodeIO = mapEncodeIO 
      (\ (File {fileType = fileType,attributes = attributes,
             name = name,simpleFile = simpleFile}) ->
         (fileType,attributes,name,simpleFile)
         )
   decodeIO codedValue0 view =
      do
         ((fileType,attributes,name,simpleFile),codedValue1) <-
            decodeIO codedValue0 view
         return (File {fileType = fileType,attributes = attributes,
             name = name,simpleFile = simpleFile},
             codedValue1)

-- ------------------------------------------------------------------
-- The instance of ObjectType
-- ------------------------------------------------------------------

theNodeType :: NodeType
theNodeType = fromString ""

instance ObjectType FileType File where
   objectTypeTypeIdPrim _ = "Files"
   objectTypeIdPrim objectType = fileTypeId objectType
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim file = fileType file
   nodeTitlePrim file = name file

   getNodeDisplayData view wrappedDisplayType fileType =
      return (
         let
            nodeTypeParmsOpt = getNodeTypeParms wrappedDisplayType 
               (displayParms fileType)
         in
            case nodeTypeParmsOpt of
               Just nodeTypeParms ->
                  Just (NodeDisplayData {
                     topLinks = [],
                     arcTypes = [],
                     nodeTypes = [(theNodeType,
                        ValueTitle (\ (str,_ :: Link File) -> return str) $$
                           nodeTypeParms
                        )],
                     getNodeType = const theNodeType,
                     knownSet = SinkSource (knownFiles fileType),
                     mustFocus = (\ _ -> return False),
                     focus = (\ link ->
                        return (SinkSource emptyVariableSet,
                           SinkSource emptyVariableSet)
                        ),
                     closeDown = done
                     })
               Nothing -> Nothing
         )              

-- ------------------------------------------------------------------
-- Getting at the SimpleFile
-- ------------------------------------------------------------------

instance HasFilePath File where
   toFilePath file = toFilePath (simpleFile file)

-- ------------------------------------------------------------------
-- The global registry and a permanently empty variable set
-- ------------------------------------------------------------------

globalRegistry :: GlobalRegistry FileType
globalRegistry = IOExts.unsafePerformIO createGlobalRegistry
{-# NOINLINE globalRegistry #-}


emptyVariableSet :: VariableSet (WrappedLink,ArcType)
emptyVariableSet = IOExts.unsafePerformIO newEmptyVariableSet
         
-- ------------------------------------------------------------------
-- newEmptyFile
-- ------------------------------------------------------------------

-- Creating a new empty file with the given name
-- We use the inputAttributes method to get the attributes, and
-- return Nothing if the user cancels.
newEmptyFile :: View -> FileType -> String -> IO (Maybe (Link File))
newEmptyFile view fileType name =
   do
      attributesOpt <- inputAttributes view (requiredAttributes fileType)
      case attributesOpt of
         Nothing -> return Nothing
         Just attributes ->
            do
               simpleFile <- newSimpleFile view
               let
                  file = File {
                     fileType = fileType,
                     attributes = attributes,
                     name = name,
                     simpleFile = simpleFile
                     }
               versioned <- createObject view file
               link <- makeLink view versioned
               return (Just link)

-- ------------------------------------------------------------------
-- Registering the file type
-- ------------------------------------------------------------------

registerFiles :: IO ()
registerFiles = 
   do
      registerObjectType (error "Unknown FileType" :: FileType)


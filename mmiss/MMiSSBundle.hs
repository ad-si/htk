{- A Bundle encodes a collection of MMiSS objects, packages,
   package folders and so on to be imported into or exported from the
   repository.  A BundleNode encodes a single object. 

   Thus the types correspond fairly closely to the (files) and (file) 
   elements, respectively, in api/MMiSSRequest.dtd.  Conversion functions are 
   in mmiss/api/MMiSSToFromBundle.hs.

   We also define the ExportOpts type, because that parallels the
   GetObject_Attrs type in api/MMiSSRequest.hs.
   -}
module MMiSSBundle (
   PackageId(..),
   Bundle(..),
   FileLoc(..),
   BundleType(..),
   BundleTypeEnum(..),
   BundleNode(..),
   BundleNodeData(..),
   BundleText(..),
   CharType(..),      
   ExportOpts(..),

   HasBundleNodeData(..),
   ) where

import Text.XML.HaXml.Types

import ICStringLen
import IntPlus

import View

import MMiSSVariant
import MMiSSFormat

-- --------------------------------------------------------------------------
-- Datatypes
-- --------------------------------------------------------------------------

newtype PackageId = PackageId String deriving (Eq,Ord)

newtype Bundle = Bundle [BundleNode]

data FileLoc = FileLoc {
   name :: Maybe String, 
      -- Nothing for the preamble
   objectType :: BundleType
   } deriving (Ord,Eq)
   
data BundleType = BundleType {
   base :: BundleTypeEnum,
   ext :: Maybe String,
   extra :: Maybe String
   } deriving (Ord,Eq)

data BundleTypeEnum = FolderEnum | FileEnum 
   | MMiSSFolderEnum | MMiSSObjectEnum | MMiSSFileEnum | MMiSSPreambleEnum
   | UnknownType
   deriving (Ord,Eq)

data BundleNode = BundleNode {
   fileLoc :: FileLoc,
   bundleNodeData :: BundleNodeData
   }

data BundleNodeData =
      Object [(Maybe MMiSSVariantSpec,BundleText)]
         -- For MMiSSObjects it will be possible to work out the variants
         -- from the BundleText; for these the variants are optional.
   |  Dir Bundle
   |  NoData
         -- data left out for some reason.

data BundleText = 
      BundleString { 
         contents :: ICStringLen,
         charType :: CharType
         }
   |  BundleElement Element
   |  NoText

data CharType = Byte | Unicode

data ExportOpts = ExportOpts {
   getText :: Bool, -- ^ Get the text of everything (not just the locations)
   format :: Format, -- ^ Way text of MMiSS objects should be presented
   recurseDepth :: IntPlus
      -- ^ Depth to recurse in fetching sub-objects (for MMiSS objects)
      -- or subdirectories (for folders).
      -- This type is more general than the corresponding attribute in
      -- MMiSSRequest.dtd, which just allows the equivalent of 1 and infinity.
   }              
   
-- --------------------------------------------------------------------------
-- Although for many object types, the BundleNodeData is constructed
-- in MMiSSExportEntireBundleNode, MMiSSFileType and MMiSSObjectType
-- prefer to do the work themselves by providing instances of
-- HasBundleNodeData.
-- --------------------------------------------------------------------------

class HasBundleNodeData object where
   getBundleNodeData :: View -> object -> ExportOpts -> IO BundleNodeData
   getBundleNodeDataForVariant :: View -> object -> ExportOpts 
      -> MMiSSVariantSearch -> IO BundleNodeData



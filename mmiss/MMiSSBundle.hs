-- | A Bundle encodes a collection of MMiSS objects, packages,
-- package folders and so on to be imported into or exported from the
-- repository.  A BundleNode encodes a single object. 
-- 
-- Thus the types correspond fairly closely to the (files) and (file) 
-- elements, respectively, in MMiSSRequest.dtd.  Conversion functions are 
-- "MMiSSToFromBundle".
-- 
-- We also define the ExportOpts type, because that parallels the
-- GetObject_Attrs type in "MMiSSRequest".
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
import Dynamics

import View

import LaTeXParser(PackageId(..))

import MMiSSVariant
import MMiSSFormat

-- --------------------------------------------------------------------------
-- Datatypes
-- We make things instance Show, for purposes of debugging.
-- --------------------------------------------------------------------------

newtype Bundle = Bundle [(PackageId,BundleNode)] deriving (Show)

data FileLoc = FileLoc {
   name :: Maybe String, 
      -- Nothing for the preamble and in some other cases, for example
      -- when we haven't worked out the name of a package yet.
   objectType :: BundleType
   } deriving (Ord,Eq,Show)
   
data BundleType = BundleType {
   base :: BundleTypeEnum,
   ext :: Maybe String,
   extra :: Maybe String
      -- extra is used for cases where the object type cannot be worked out
      -- otherwise; specifically for plain files and plain folders.
   } deriving (Ord,Eq,Show)

data BundleTypeEnum = FolderEnum | FileEnum 
   | MMiSSFolderEnum | MMiSSObjectEnum | MMiSSFileEnum | MMiSSPreambleEnum
   | UnknownType
   deriving (Ord,Eq,Show)

data BundleNode = BundleNode {
   fileLoc :: FileLoc,
   bundleNodeData :: BundleNodeData
   } deriving (Show)

data BundleNodeData =
      Object [(Maybe MMiSSVariantSpec,BundleText)]
         -- For MMiSSObjects it will be possible to work out the variants
         -- from the BundleText; for these the variants are optional.
   |  Dir [BundleNode]
   |  NoData
         -- data left out for some reason.
   deriving (Show)

data BundleText = 
      BundleString {
         contents :: ICStringLen,
         charType :: CharType
            -- how to write this.
         }
   |  BundleDyn {
         dyn :: Dyn,
            -- this can be used to stash a pre-parsed element,
            -- for example an Element or MMiSSLatexPreamble.
         eqFn :: Dyn -> Bool
            -- returns True if this element is equal to the given one.
         }
   |  NoText

instance Show BundleText where
   show (BundleString {contents = icsl,charType = charType}) =
      "BundleString {contents = " 
      ++ show icsl
      ++ ",charType = "
      ++ show charType
      ++ "}"
   show (BundleDyn {dyn = dyn}) =
      "BundleDyn {dyn = "
       ++ show dyn
       ++ ",eqFn = ???}"
   show NoText = "NoText"

data CharType = Byte | Unicode deriving (Eq,Show)

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



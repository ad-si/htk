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

   bundleToICSL, -- :: BundleText -> (ICStringLen,CharType)
   bundleToElement, -- :: BundleText -> WithError Element
   nameFileLoc, -- :: FileLoc -> WithError EntityName
   describeFileLoc, -- :: FileLoc -> String

   getUnknownBundleNode, -- :: LinkedObject -> IO BundleNode
   ) where

import Maybe

import Data.FiniteMap

import Text.XML.HaXml.Types

import ICStringLen
import AtomString
import UTF8
import Computation

import EntityNames

import LinkManager

import LaTeXParser(MMiSSLatexPreamble)

import MMiSSVariant
import MMiSSDTD
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

data BundleText = 
      BundleString { 
         contents :: ICStringLen,
         charType :: CharType
         }
   |  BundleElement Element

data CharType = Byte | Unicode

data ExportOpts = ExportOpts {
   getText :: Bool, -- ^ Get the text of everything (not just the locations)
   format :: Format, -- ^ Way text of MMiSS objects should be presented
   recurse :: Bool
      -- ^ get sub objects (for MMiSS objects) or subdirectories
      -- (for folders).
   }              
   

-- --------------------------------------------------------------------------
-- BundleText functions
-- --------------------------------------------------------------------------

bundleToICSL :: BundleText -> (ICStringLen,CharType)
bundleToICSL (BundleString {contents = contents,charType = charType}) =
   (contents,charType)
bundleToICSL (BundleElement element) = 
   (fromString (toExportableXml element),Byte)
 

bundleToElement :: BundleText -> WithError Element
bundleToElement (BundleElement element) = return element
bundleToElement (BundleString {contents = contents,charType = charType}) =
   do
      contentsStr <- case charType of
         Byte -> return (toString contents)
         Unicode -> fromUTF8WE (toString contents)
      xmlParseWE "MMiSS Bundle" contentsStr

-- --------------------------------------------------------------------------
-- FileLoc functions
-- --------------------------------------------------------------------------

strFileLoc :: FileLoc -> Maybe String
strFileLoc fileLoc =
   case name fileLoc of
      Nothing -> Nothing
      Just name0 -> case ext (objectType fileLoc) of
         Nothing -> Just name0
         Just ext0 -> Just (name0 ++ [specialChar] ++ ext0)

nameFileLoc :: FileLoc -> WithError EntityName
nameFileLoc fileLoc = 
   do
      str <- case strFileLoc fileLoc of
         Nothing -> fail "Attempt to write file where no name is specified"
         Just str -> return str
      fromStringWE str

describeFileLoc :: FileLoc -> String
describeFileLoc fileLoc =
   fromMaybe
      (fallBack fileLoc)
      (strFileLoc fileLoc)
   where
      fallBack :: FileLoc -> String
      fallBack fileLoc =
         case ext (objectType fileLoc) of
            Just ext1 -> "Unnamed object with extension " ++ ext1
          

describeBundleTypeEnum :: BundleTypeEnum -> String
describeBundleTypeEnum bte = case bte of
   FolderEnum -> "simple folder"
   FileEnum -> "simple file"
   MMiSSFolderEnum -> "MMiSS package folder"
   MMiSSObjectEnum -> "MMiSS object"
   MMiSSFileEnum -> "MMiSS file"
   MMiSSPreambleEnum -> "MMiSS preamble"
   UnknownType -> "Object of unknown type"

-- --------------------------------------------------------------------------
-- Miscellaneous functions
-- --------------------------------------------------------------------------

-- BundleNode extraction in the case when we can't do the job properly and
-- need a soft fall-out.
getUnknownBundleNode :: LinkedObject -> IO BundleNode
getUnknownBundleNode linkedObject =
   do
      insertionOpt <- getCurrentInsertion linkedObject
      let
         name1 = fmap
            (toString . snd . unmkInsertion)
            insertionOpt

         objectType1 = BundleType {
            base = UnknownType,
            ext = Nothing,
            extra = Nothing
            }

         fileLoc1 = FileLoc {name = name1, objectType = objectType1}

         bundleNode = BundleNode {
            fileLoc = fileLoc1,
            bundleNodeData = Object []
            }
      return bundleNode
 

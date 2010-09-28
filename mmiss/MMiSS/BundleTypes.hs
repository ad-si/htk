-- | Various types we need to know for constructing Bundles.
module MMiSS.BundleTypes(
   mmissPreambleType,
      -- :: BundleType
      -- type used for MMiSSPreambles
   mmissPackageType,
      -- :: BundleType
      -- type used for MMiSSPackages
   mmissObjectType,
      -- :: BundleType
      -- type used for MMiSSObjects
   mmissSubFolderType,
      -- :: BundleType
      -- type used for MMiSSSubFolders.
   mmissFolderType,
      -- :: BundleType
      -- type used for MMiSSFolders.
   mmissObjectAsXMLType,
      -- :: BundleType
      -- type used for MMiSSObjects when they are in a bundle as XML text.
   mmissObjectAsTeXType,
      -- :: BundleType
      -- type used for MMiSSObjects when they are in a bundle as LaTeX text.
   mmissObjectAsFileType,
      -- :: Format -> BundleType
      -- generalisation of mmissObjectAsXMLType and mmissObjectAsTeXType

   mmissFileType,
      -- :: String -> BundleType
      -- type used for MMiSSFiles with a given extension.
   ) where

import Util.AtomString

import Types.ObjectTypes

import MMiSS.Bundle
import MMiSS.Format
import qualified Types.Folders as Folders
import qualified MMiSS.SubFolder as MMiSSSubFolder

mmissPreambleType :: BundleType
mmissPreambleType = BundleType {
   base = MMiSSPreambleEnum,
   ext = Nothing,
   extra = Nothing
   }

mmissPackageType :: BundleType
mmissPackageType = BundleType {
   base = MMiSSFolderEnum,
   ext = Nothing,
   extra = Nothing
   }

mmissObjectType :: BundleType
mmissObjectType = BundleType {
   base = MMiSSObjectEnum,
   ext = Nothing,
   extra = Nothing
   }

mmissSubFolderType :: BundleType
mmissSubFolderType = BundleType {
   base = FolderEnum,
   ext = Nothing,
   extra = Just (toString (
      objectTypeIdPrim (MMiSSSubFolder.mmissSubFolderType)))
   }

mmissFolderType :: BundleType
mmissFolderType = BundleType {
   base = FolderEnum,
   ext = Nothing,
   extra = Just (toString (
      objectTypeIdPrim (Folders.plainFolderType)))
   }

mmissObjectAsFileType :: Format -> BundleType
mmissObjectAsFileType format = BundleType {
   base = MMiSSFileEnum,
   ext = Just (toExtension format),
   extra = Nothing
   }

mmissObjectAsXMLType :: BundleType
mmissObjectAsXMLType = mmissObjectAsFileType XML

mmissObjectAsTeXType :: BundleType
mmissObjectAsTeXType = mmissObjectAsFileType LaTeX


mmissFileType :: String -> BundleType
mmissFileType ext = BundleType {
   base = MMiSSFileEnum,
   ext = Just ext,
   extra = Nothing
   }

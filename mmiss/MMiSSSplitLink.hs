{- This module contains code for turning a LinkedObject or a WrappedLink
   into one of the types known to us.
   -}
module MMiSSSplitLink(
   SplitLink(..),
   splitWrappedLink,
   splitLinkedObject,
   ) where

import ObjectTypes

import Folders
import Files
import LinkManager
import Link

import MMiSSPreamble
import MMiSSPackageFolder
import MMiSSObjectType
import MMiSSFileType

-- ------------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------------

data SplitLink =
      FileC (Link File)
   |  FolderC (Link Folder)
   |  MMiSSPreambleC (Link MMiSSPreamble)
   |  MMiSSPackageFolderC (Link MMiSSPackageFolder)
   |  MMiSSObjectC (Link MMiSSObject)
   |  MMiSSFileC (Link MMiSSFile)
   |  UnknownLinkC

-- ------------------------------------------------------------------------
-- The functions
-- ------------------------------------------------------------------------

splitWrappedLink :: WrappedLink -> SplitLink
splitWrappedLink w =
   case (unpackWrappedLink w,unpackWrappedLink w,unpackWrappedLink w,
         unpackWrappedLink w,unpackWrappedLink w,unpackWrappedLink w) of
      (Just l,_,_,_,_,_) -> FileC l
      (_,Just l,_,_,_,_) -> FolderC l 
      (_,_,Just l,_,_,_) -> MMiSSPreambleC l
      (_,_,_,Just l,_,_) -> MMiSSPackageFolderC l 
      (_,_,_,_,Just l,_) -> MMiSSObjectC l 
      (_,_,_,_,_,Just l) -> MMiSSFileC l 
      (_,_,_,_,_,_) -> UnknownLinkC

splitLinkedObject :: LinkedObject -> SplitLink
splitLinkedObject = splitWrappedLink . toWrappedLink
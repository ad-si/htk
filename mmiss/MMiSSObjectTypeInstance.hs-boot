module MMiSSObjectTypeInstance where

import {-# SOURCE #-} MMiSSPackageFolder
import MMiSSObjectType
import ViewType
import Link
import LinkManager
import ObjectTypes
import EntityNames
import Computation
import Data.Maybe
import GHC.IOBase

unpackWrappedLinkToMMiSSObject 
   :: ObjectTypes.WrappedLink 
   -> Data.Maybe.Maybe (Link.Link MMiSSObjectType.MMiSSObject)

newEmptyLinkMMiSSObject 
   :: ViewType.View 
   -> ObjectTypes.WrappedLink 
   -> GHC.IOBase.IO (Link.Link MMiSSObjectType.MMiSSObject)


wrapMMiSSObjectLink
   :: Link.Link MMiSSObjectType.MMiSSObject -> ObjectTypes.WrappedLink

linkToLinkedObjectMMiSSObject
   :: ViewType.View -> Link.Link MMiSSObjectType.MMiSSObject 
   -> GHC.IOBase.IO LinkManager.LinkedObject

lookupMMiSSObject 
   :: ViewType.View 
   -> MMiSSPackageFolder.MMiSSPackageFolder 
   -> EntityNames.EntitySearchName
   -> GHC.IOBase.IO (Computation.WithError (
      Data.Maybe.Maybe (Link.Link MMiSSObjectType.MMiSSObject)))

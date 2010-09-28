module MMiSS.ObjectTypeInstance where

import {-# SOURCE #-} MMiSS.PackageFolder as MMiSSPackageFolder
import MMiSS.ObjectType as MMiSSObjectType
import Types.ViewType as ViewType
import Types.Link as Link
import Types.LinkManager as LinkManager
import Types.ObjectTypes as ObjectTypes
import Imports.EntityNames as EntityNames
import Util.Computation as Computation
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

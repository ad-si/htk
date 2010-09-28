module MMiSS.PackageFolder where

import Types.LinkManager as LinkManager
import Types.ViewType as ViewType
import Util.Computation as Computation
import Util.Sources as Sources
import Types.Link as Link
import Imports.EntityNames as EntityNames
import MMiSS.Preamble as MMiSSPreamble
import Types.ObjectTypes as ObjectTypes
import Data.Maybe
import GHC.IOBase

data MMiSSPackageFolder

getMMiSSPackageFolder ::
   LinkManager.HasLinkedObject object
   => ViewType.View 
   -> object 
   -> GHC.IOBase.IO (Computation.WithError MMiSSPackageFolder)

getMMiSSPackageFolderAndName :: 
   LinkManager.HasLinkedObject object 
   => ViewType.View 
   -> object 
   -> GHC.IOBase.IO (Computation.WithError (
      MMiSSPackageFolder,EntityNames.EntityFullName))


toMMiSSPreambleLink ::
   MMiSSPackageFolder 
   -> Link.Link MMiSSPreamble.MMiSSPreamble

toMMiSSPackageFolderLinkedObject 
   :: MMiSSPackageFolder -> LinkManager.LinkedObject

toMMiSSPackageFolder :: 
   LinkManager.HasLinkedObject object 
   => ViewType.View 
   -> object 
   -> Sources.SimpleSource (Computation.WithError MMiSSPackageFolder)


unpackWrappedLinkToMMiSSPackageFolder 
   :: ObjectTypes.WrappedLink 
   -> Data.Maybe.Maybe (Link.Link MMiSSPackageFolder)

wrapMMiSSPackageFolderLink :: Link.Link MMiSSPackageFolder -> ObjectTypes.WrappedLink


newEmptyLinkMMiSSPackageFolder
   :: ViewType.View 
   -> ObjectTypes.WrappedLink 
   -> GHC.IOBase.IO (Link.Link MMiSSPackageFolder)

linkToLinkedObjectMMiSSPackageFolder
   :: ViewType.View -> Link.Link MMiSSPackageFolder 
   -> GHC.IOBase.IO LinkManager.LinkedObject

module MMiSSPackageFolder where

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
   :: ViewType.View -> GHC.IOBase.IO (Link.Link MMiSSPackageFolder)

linkToLinkedObjectMMiSSPackageFolder
   :: ViewType.View -> Link.Link MMiSSPackageFolder 
   -> GHC.IOBase.IO LinkManager.LinkedObject

module MMiSSPackageFolder where

data MMiSSPackageFolder

getMMiSSPackageFolder ::
   LinkManager.HasLinkedObject object
   => ViewType.View 
   -> object 
   -> GHC.IOBase.IO (Computation.WithError MMiSSPackageFolder)

toMMiSSPreambleLink ::
   MMiSSPackageFolder 
   -> Link.Link MMiSSPreamble.MMiSSPreamble


toMMiSSPackageFolderLinkedObject 
   :: MMiSSPackageFolder -> LinkManager.LinkedObject

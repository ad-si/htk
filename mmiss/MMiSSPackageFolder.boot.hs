module MMiSSPackageFolder where

data MMiSSPackageFolder

getMMiSSPackageFolder ::
   ViewType.View 
   -> LinkManager.LinkedObject 
   -> GHC.IOBase.IO (Computation.WithError MMiSSPackageFolder)

toMMiSSPreambleLink ::
   MMiSSPackageFolder 
   -> Link.Link MMiSSPreamble.MMiSSPreamble


toLinkEnvironment :: MMiSSPackageFolder -> LinkManager.LinkEnvironment

module VersionState where

data VersionState 

thisServerId :: VersionState -> GHC.Base.String

versionIsAncestor :: VersionState 
   -> VersionInfo.ObjectVersion 
   -> VersionInfo.ObjectVersion 
   -> GHC.IOBase.IO GHC.Base.Bool

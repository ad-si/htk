module VersionGraphClient where

getVersionInfo 
   :: VersionGraphClient 
   -> VersionInfo.ObjectVersion 
   -> GHC.IOBase.IO VersionInfo.VersionInfo

data VersionGraphClient
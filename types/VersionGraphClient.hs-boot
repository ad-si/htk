module VersionGraphClient where

import VersionInfo

getVersionInfo 
   :: VersionGraphClient 
   -> ObjectVersion 
   -> IO VersionInfo

data VersionGraphClient
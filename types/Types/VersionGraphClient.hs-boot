module Types.VersionGraphClient where

import SimpleDB.VersionInfo

getVersionInfo 
   :: VersionGraphClient 
   -> ObjectVersion 
   -> IO VersionInfo

data VersionGraphClient

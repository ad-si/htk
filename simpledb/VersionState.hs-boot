module VersionState where

import ObjectVersion
data VersionState 

thisServerId :: VersionState -> String

versionIsAncestor :: VersionState 
   -> ObjectVersion 
   -> ObjectVersion 
   -> IO Bool

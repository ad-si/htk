module SimpleDB.VersionState where

import SimpleDB.ObjectVersion
data VersionState 

thisServerId :: VersionState -> String

versionIsAncestor :: VersionState 
   -> ObjectVersion 
   -> ObjectVersion 
   -> IO Bool

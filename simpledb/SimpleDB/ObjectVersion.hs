-- | This module provides the type used in VersionInfo and VersionState.hs-boot
module SimpleDB.ObjectVersion where

import Util.Dynamics
import Util.DeepSeq

newtype ObjectVersion = ObjectVersion Integer
   deriving (Eq,Ord,Typeable,DeepSeq,Enum)
   -- Type of handle referring to a version.

{- CVSTypes contains a couple of types and corresponding instance 
   for the CVS implementation. 
   -}
module CVSTypes(
   CVSFile(..), -- alias for file.  A newtype for String
   CVSVersion(..) -- alias for version.  Ditto.
   ) where

import AtomString
import Dynamics

newtype CVSFile = CVSFile String deriving (Eq,Ord)

instance StringClass CVSFile where
   toString (CVSFile str) = str
   fromString str = CVSFile str

cvsFile_tyCon = mkTyCon "CVSTypes" "CVSFile"
instance HasTyCon CVSFile where
   tyCon _ = cvsFile_tyCon

newtype CVSVersion = CVSVersion String deriving (Eq,Ord)

instance StringClass CVSVersion where
   toString (CVSVersion str) = str
   fromString str = CVSVersion str

cvsVersion_tyCon = mkTyCon "CVSTypes" "CVSVersion"
instance HasTyCon CVSVersion where
   tyCon _ = cvsVersion_tyCon

 



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

cvsFile_tyRep = mkTyRep "CVSTypes" "CVSFile"
instance HasTyRep CVSFile where
   tyRep _ = cvsFile_tyRep

newtype CVSVersion = CVSVersion String deriving (Eq,Ord)

instance StringClass CVSVersion where
   toString (CVSVersion str) = str
   fromString str = CVSVersion str

cvsVersion_tyRep = mkTyRep "CVSTypes" "CVSVersion"
instance HasTyRep CVSVersion where
   tyRep _ = cvsVersion_tyRep

 



{- CVSTypes contains a couple of types for the CVS implementation. 
   This particular file should be suitable for Hugs.
   -}
module CVSTypes(
   CVSFile(..), -- alias for file.  A newtype for String
   CVSVersion(..) -- alias for version.  Ditto.
   ) where

import AtomString

newtype CVSFile = CVSFile String deriving (Eq,Ord)

instance StringClass CVSFile where
   toString (CVSFile str) = str
   fromString str = CVSFile str

newtype CVSVersion = CVSVersion String deriving (Eq,Ord)

instance StringClass CVSVersion where
   toString (CVSVersion str) = str
   fromString str = CVSVersion str

 



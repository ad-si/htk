{- CVSTypes contains a couple of types for the CVS implementation. 
   This particular file should be suitable for Hugs.
   -}
module CVSTypes(
   CVSFile(..), -- alias for file.  A newtype for String
   CVSVersion(..) -- alias for version.  Ditto.
   ) where

import QuickReadShow

newtype CVSFile = CVSFile String deriving (Eq,Ord)
instance QuickRead CVSFile where
   quickRead = WrapRead (\ str -> CVSFile str)

instance QuickShow CVSFile where
   quickShow = WrapShow (\ (CVSFile str) -> str)

newtype CVSVersion = CVSVersion String deriving (Eq,Ord)
instance QuickRead CVSVersion where
   quickRead = WrapRead (\ str -> CVSVersion str)

instance QuickShow CVSVersion where
   quickShow = WrapShow (\ (CVSVersion str) -> str)

 



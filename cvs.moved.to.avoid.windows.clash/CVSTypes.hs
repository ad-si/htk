{- CVSTypes contains a couple of types for the CVS implementation. 
   This particular file should be suitable for Hugs.
   -}
module CVSTypes(
   CVSFile(..), -- alias for file.  A newtype for String
   CVSVersion(..) -- alias for version.  Ditto.
   ) where


newtype CVSFile = CVSFile String deriving (Eq,Ord)
instance Show CVSFile where
   showsPrec prec (CVSFile str) acc = showsPrec prec str acc

instance Read CVSFile where
   readsPrec prec str =
      let
         parses = readsPrec prec str :: [(String,String)] 
      in
         map
            (\ (result,rest) -> (CVSFile result,rest))
            parses

newtype CVSVersion = CVSVersion String deriving (Eq,Ord)
 
instance Show CVSVersion where
   showsPrec prec (CVSVersion str) acc = showsPrec prec str acc

instance Read CVSVersion where
   readsPrec prec str =
      let
         parses = readsPrec prec str :: [(String,String)] 
      in
         map
            (\ (result,rest) -> (CVSVersion result,rest))
            parses



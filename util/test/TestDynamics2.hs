module Main(main) where

import Dynamics

data Folder = Folder {
   a :: Int
   } deriving (Typeable)

data Versioned x = Versioned {
   loc :: Bool,
   s :: [x]
   }

versioned_tyRep = mkTyRep "View" "Versioned"

instance HasTyRep1 Versioned where
   tyRep1 _ = versioned_tyRep

main =
   do
      putStrLn (show (typeOf (undefined :: Versioned Folder)))




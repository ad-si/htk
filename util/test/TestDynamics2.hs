module Main(main) where

import Dynamics

data Folder = Folder {
   a :: Int
   }


folder_tyRep = mkTyRep "Folders" "Folder"
instance HasTyRep Folder where
   tyRep _ = folder_tyRep

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





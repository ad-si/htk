module Main(main) where

import Dynamics

data Folder = Folder {
   a :: Int
   }


folder_tyCon = mkTyCon "Folders" "Folder"
instance HasTyCon Folder where
   tyCon _ = folder_tyCon

data Versioned x = Versioned {
   loc :: Bool,
   s :: [x]
   }

versioned_tyCon = mkTyCon "View" "Versioned"

instance HasTyCon1 Versioned where
   tyCon1 _ = versioned_tyCon

main =
   do
      putStrLn (show (typeOf (undefined :: Versioned Folder)))





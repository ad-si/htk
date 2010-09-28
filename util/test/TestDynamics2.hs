module Main(main) where

import Util.Dynamics

data Folder = Folder {
   a :: Int
   } deriving (Typeable)

data Versioned x = Versioned {
   loc :: Bool,
   s :: [x]
   } deriving (Typeable)

main =
   do
      putStrLn (show (typeOf (undefined :: Versioned Folder)))




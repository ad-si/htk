{- --------------------------------------------------------------------
 -
 - Module: Name
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module Name (

  Name(..),
  createName,
  getFullName,
  getShortName

) where

data Name = Name { short :: Int -> String,
	           full  :: String }

shortdef :: String -> Int -> String
shortdef str i =
  if length str > i then take (length str + 2 - i) str ++ ".." else str

createName :: String -> Name
createName str = Name { short = shortdef str, full = str }

getFullName :: Name -> String
getFullName nm = full nm

getShortName :: Name -> Int -> String
getShortName nm i = short nm i

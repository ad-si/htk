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
  newName,
  getFullName,
  getShortName

) where

data Name = Name { short :: Int -> String,
	           full  :: String }

shortdef :: String -> Int -> String
shortdef str i =
  if length str < 5 then take i str
  else take 2 str ++ ".." ++ drop (i - 4) str

newName :: String -> Name
newName str = Name { short = shortdef str, full = str }

getFullName :: Name -> String
getFullName nm = full nm

getShortName :: Name -> Int -> String
getShortName nm i = short nm i
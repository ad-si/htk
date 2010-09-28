-- | This module exports a common interface for named objects.
module HTk.Toolkit.Name (

  Name(..),
  createName,
  getFullName,
  getShortName

) where

-- | The @Name@ datatype.
data Name = Name { short :: Int -> String,
                   full  :: String }

shortdef :: String -> Int -> String
shortdef str i =
  if length str > i then take (i - 2) str ++ ".." else str

-- | Creates a new name.
createName :: String
   -- ^ the full name.
   -> Name
   -- ^ A name.
createName str = Name { short = shortdef str, full = str }

-- | Gets the full name from a @Name@ object.
getFullName :: Name -> String
getFullName nm = full nm

-- | Gets a short name of the given length from a @Name@ object.
getShortName :: Name -> Int -> String
getShortName nm i = short nm i

{- CodedValueStore controls the actual storage and retrieval
   of coded values for storage in the repository. 

   In this file we also provide
      instance StringClass CodedValue
   and
      instance HasCodedValue CodedValue
   -}
module CodedValueStore(
   CodedValue(..),
   toObjectSource, -- :: CodedValue -> IO ObjectSource
   -- CodedValue is also made an instance of StringClass
   fromObjectSource, -- :: ObjectSource -> IO CodedValue
   ) where

import AtomString(StringClass(..))
import Dynamics

import VersionDB

---------------------------------------------------------------------
-- The CodedValue type.
---------------------------------------------------------------------

newtype CodedValue = CodedValue [Maybe Char] deriving (Eq)
-- Why not just String?  Because CVS does versioning better when data
-- is considered in a record-based way.  Thus we try to represent each
-- "large" bit of data, IE more than 2 or 3 characters or so,
-- on a separate record.  In this list, Nothing
-- represents the record boundaries. 

---------------------------------------------------------------------
-- We go via strings.
---------------------------------------------------------------------

instance StringClass CodedValue where
   toString (CodedValue mList) = toString' mList
      where
         toString' [] = []
         toString' (Nothing : rest) = '\n':(toString' rest)
         toString' (Just '\\' : rest) = '\\':'\\':(toString' rest)
         toString' (Just '\n' : rest) = '\\':'n':(toString' rest)
         toString' (Just ch : rest) = ch:(toString' rest)
    
   fromString str = CodedValue (fromString' str) 
      where
         fromString' [] = []
         fromString' ('\n':rest) = Nothing : (fromString' rest)
         fromString' ('\\':'\\' : rest) = Just '\\' : (fromString' rest)
         fromString' ('\\':'n' : rest) = Just '\n' : (fromString' rest)
         fromString' ('\\':_ : rest) = error 
            "Unexpected escape in CodedValueStore.fromString"
         fromString' (ch : rest) = Just ch : (fromString' rest)

---------------------------------------------------------------------
-- Convert strings to objects
---------------------------------------------------------------------

toObjectSource :: CodedValue -> IO ObjectSource
toObjectSource codedValue = 
   do
      let str = toString codedValue
      importString str

fromObjectSource :: ObjectSource -> IO CodedValue
fromObjectSource objectSource =
   do
      str <- exportString objectSource
      return (fromString str)



{- CodedValueStore controls the actual storage and retrieval
   of coded values for storage in the repository. -}
module CodedValueStore(
   toObjectSource, -- :: CodedValue -> IO ObjectSource
   -- CodedValue is also made an instance of StringClass
   fromObjectSource, -- :: ObjectSource -> IO CodedValue
   ) where

import AtomString(StringClass(..))

import VersionDB
import CodedValue

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

{- CodedValueStore controls the actual storage and retrieval
   of coded values for storage in the repository. 

   In this file we also provide
      instance StringClass CodedValue
   and
      instance HasCodedValue CodedValue
   -}
module CodedValueStore(
   toObjectSource, -- :: CodedValue -> IO ObjectSource
   -- CodedValue is also made an instance of StringClass
   fromObjectSource, -- :: ObjectSource -> IO CodedValue
   ) where

import AtomString(StringClass(..))
import Dynamics

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

---------------------------------------------------------------------
-- CodedValue is an instance of HasCodedValue
---------------------------------------------------------------------

codedValue_tyCon = mkTyCon "CodedValue" "CodedValue"

instance HasTyCon CodedValue where
   tyCon _ = codedValue_tyCon

instance HasCodedValue CodedValue where
   encodeIO = mapEncodeIO (\ codedValue -> Str codedValue)
   decodeIO = mapDecodeIO (\ (Str codedValue) -> codedValue)


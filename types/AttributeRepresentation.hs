{- AttributeRepresentation encodes the representation of attributes in
   a fairly efficient way. 
   -}
module AttributeRepresentation(
   CodedValue(..),
      -- CodedValue represents is used for representing a 
      -- value in a uniform way, in such a way that it can be turned
      -- into CVS files and the like.

   HasCodedValue(..),
   -- class of things we can represent with CodedValue

   -- doEncode and doDecode convert a single value (as opposed to the
   -- functions in CodedValue, which are cumulative).
   doEncode, -- :: HasCodedValue value => value -> CodedValue
   doDecode, -- :: HasCodedValue value => CodedValue -> value

   HasConverter(..),
      -- HasConverter is used to make defining CodedValue instances easier,
      -- for examples see this file.
   FormatError(..),
      -- A FormatError is thrown (using throwDyn) when for some reason
      -- the message can't be decoded.
      -- Hence you can catch errors cleanly using catchDyn.
   ) where

import Bits
import Int
import Char
import Exception

import Dynamics

---------------------------------------------------------------------
-- HashedValue's and operations on them.
---------------------------------------------------------------------

newtype CodedValue = CodedValue [Maybe Char]
-- Why not just String?  Because CVS does versioning better when data
-- is considered in a record-based way.  Thus we try to represent each
-- "large" bit of data, IE more than 2 or 3 characters or so,
-- on a separate record.  In this list, Nothing
-- represents the record boundaries. 

emptyCodedValue :: CodedValue
emptyCodedValue = CodedValue []

isEmptyCodedValue :: CodedValue -> Bool
isEmptyCodedValue (CodedValue []) = True
isEmptyCodedValue _ = False

addBoundary :: CodedValue -> CodedValue
addBoundary (CodedValue l) = CodedValue (Nothing : l)

removeBoundary :: CodedValue -> CodedValue
removeBoundary (CodedValue (Nothing : rest)) = CodedValue rest
removeBoundary _ = formatError "Record does not end where expected"

class HasCodedValue value where
   encode :: value -> CodedValue -> CodedValue
   decode :: CodedValue -> (value,CodedValue)

doEncode :: HasCodedValue value => value -> CodedValue
doEncode value = encode value (emptyCodedValue)

doDecode :: HasCodedValue value => CodedValue -> value
doDecode codedValue =
   let
      (value,rest) = decode codedValue
   in
      if isEmptyCodedValue rest
         then
            value
         else
            formatError "Extra trailing junk"

-- HasConverter is used to make defining HasCodedValue instances easier.
class HasConverter value1 value2 where
   encode' :: value1 -> value2
   decode' :: value2 -> value1

instance (HasCodedValue value1,HasCodedValue value2) 
   => HasCodedValue (value1,value2)
      where
   encode (value1,value2) codedValue = encode value1 (encode value2 codedValue)
   decode codedValue0 =
      let
         (value1,codedValue1) = decode codedValue0
         (value2,codedValue2) = decode codedValue1
      in
         ((value1,value2),codedValue2)

instance (HasCodedValue value1,HasCodedValue value2) 
   => HasCodedValue (Either value1 value2)
      where
   encode (Left value1) soFar =
      encode 'L' (encode value1 soFar)
   encode (Right value2) soFar =
      encode 'R' (encode value2 soFar)
   decode codedValue0 =
      case decode codedValue0 of
         ('L',codedValue1) ->
            let
               (value1,codedValue2) = decode codedValue1
            in
               (Left value1,codedValue2)
         ('R',codedValue1) ->
            let
               (value2,codedValue2) = decode codedValue1
            in
               (Right value2,codedValue2)
         _ -> formatError "Unexpected character decoding Either"

instance HasCodedValue value => HasCodedValue (Maybe value) where
   encode value codedValue = 
      encode (encode' value :: Either value ()) codedValue
   decode codedValue0 =
      let
         (value' :: Either value (),codedValue1) = decode codedValue0
      in
         (decode' value',codedValue1)

instance HasConverter (Maybe value) (Either value ()) where
   encode' (Just value) = Left value
   encode' Nothing = Right ()

   decode' (Left value) = Just value
   decode' (Right ()) = Nothing

instance HasCodedValue value => HasCodedValue [value]
      where
   -- Lists being large items, we put a "Nothing" before
   -- each entry.
   encode values codedValue0 =
      let
         l = length values
         codedValue1 = 
            foldr
               (\ value codedValue -> addBoundary (encode value codedValue))
               codedValue0
               values
      in
         encode l codedValue1
   decode codedValue0 =
      let
         (l :: Int,codedValue1) = decode codedValue0
         decodeNValues :: Int -> CodedValue -> ([value],CodedValue)
         decodeNValues n codedValue0 =
            if n<=0 
               then
                  if n<0
                     then 
                        formatError "Bad list length in AttributeTypes.decode"
                     else
                        ([],codedValue0)
               else
                  let
                     (value1,codedValue1) = decode (removeBoundary codedValue0)
                     (values,codedValue2) = decodeNValues (n-1) codedValue1
                  in
                     (value1 : values,codedValue2)
      in
         decodeNValues l codedValue1

---------------------------------------------------------------------
-- Particular primitive types, apart from Int
---------------------------------------------------------------------

instance HasCodedValue () where
   -- used in fact for encoding Maybe.
   encode () codedValue = codedValue
   decode codedValue = ((),codedValue)

instance HasCodedValue Char where
   encode ch (CodedValue soFar) = CodedValue ((Just ch) : soFar)
   decode (CodedValue (Just ch : rest)) = (ch,CodedValue rest)
   decode _ = formatError "Decoding char, found an unexpected record end"

-- Note that this overlapping instance overrules the more general one via
-- HasCodedValue Char.
instance HasCodedValue String where
   encode str (CodedValue soFar) = 
      CodedValue ((map Just str) ++ (Nothing:soFar))
   decode (CodedValue value) = dec value
      where
         dec :: [Maybe Char] -> (String,CodedValue)
         dec (Nothing:rest) = ("",CodedValue rest)
         dec (Just ch:rest) =
            let
               (str,restCodedValue) = dec rest
            in
               (ch:str,restCodedValue)

instance HasCodedValue Bool where
   encode value codedValue = encode (encode' value :: Char) codedValue
   decode codedValue0 =
      let
         (value' :: Char,codedValue1) = decode codedValue0
      in
         (decode' value',codedValue1)

instance HasConverter Bool Char where
   encode' True = 'T'
   encode' False = 'F'
   
   decode' 'T' = True
   decode' 'F' = False
   decode' _ = formatError "Format error reading Bool"

---------------------------------------------------------------------------
-- Encoding Ints 
-- We try to do this economically so that small integers have small
-- representations, and so on.  We are also hamstrung because for some
-- bizarre reason Glasgow Haskell does not define Bits for Int.
---------------------------------------------------------------------------

bitsInChar :: Int 
-- Number of bits easily stored in the Char type.  Thus if Unicode ends
-- up getting stored as UTF8 we may prefer to change this to 7 or 16.
bitsInChar = 8

bitsPerChar :: Int
-- Number of bits of an integer we will store per char.
-- (The remaining one is used to mark the end of the sequence.)
bitsPerChar = bitsInChar - 1

-- Here are some useful abbreviations in this connection
topBit :: Bits integral => integral
topBit = bit bitsPerChar

mask :: (Integral integral,Bits integral) => integral
mask = topBit - 1

nextBit :: Bits integral => integral
nextBit = bit (bitsInChar - 2)

newtype CodedList = CodedList [Int32]
-- This is a nonempty list of integers in [0,2^(bitsInChar-1)).

chrGeneral :: Integral a => a -> Char
chrGeneral value = chr (fromIntegral value)

ordGeneral :: Integral a => Char -> a
ordGeneral value = fromIntegral (ord value)

instance HasCodedValue CodedList where
   encode (CodedList [i]) codedValue =
      encode (chrGeneral (i .|. topBit)) codedValue
   encode (CodedList (x:xs)) codedValue =
      encode (chrGeneral x) (encode (CodedList xs) codedValue)

   decode codedValue =
      let
         (ch,nextCodedValue) = decode codedValue 
         i = ordGeneral ch
      in
         if i<topBit
            then
               let
                  (CodedList xs,rest) = decode nextCodedValue
               in
                  (CodedList (i:xs),rest)
            else
               (CodedList [i `xor` topBit],nextCodedValue)

instance (Integral integral,Bits integral) => HasCodedValue integral where
   encode value codedValue = encode (encode' value :: CodedList) codedValue
   decode codedValue0 =
      let
         (value' :: CodedList,codedValue1) = decode codedValue0
      in
         (decode' value',codedValue1)

instance (Integral integral,Bits integral) 
   => HasConverter integral CodedList 
      where
   encode' i =
      if (i >= nextBit) || (i < -nextBit) 
         then
            let
               lowestPart = i .&. mask
               highPart = i `shiftR` bitsPerChar
               CodedList codedHigh = encode' highPart 
            in
               CodedList ((fromIntegral lowestPart) : codedHigh)
         else
            let
               wrapped =
                  if i < 0
                     then
                        topBit + i
                     else
                        i
            in
               CodedList [fromIntegral wrapped]
   decode' (CodedList [wpped]) =
      let
         wrapped = fromIntegral wpped
      in
         if wrapped >= nextBit
            then
               wrapped - topBit
            else
               wrapped
   decode' (CodedList (lPart : codedHigh)) =
      let
         lowestPart = fromIntegral lPart
         highPart = decode' (CodedList codedHigh) 
      in
         lowestPart + (highPart `shiftL` bitsPerChar)

instance HasCodedValue Int where
   encode value codedValue = encode (encode' value :: Int32) codedValue
   decode codedValue0 =
      let
         (value' :: Int32,codedValue1) = decode codedValue0
      in
         (decode' value',codedValue1)

instance HasConverter Int Int32 where
   encode' = fromIntegral
   decode' = fromIntegral

---------------------------------------------------------------------
-- FormatError's
---------------------------------------------------------------------

newtype FormatError = FormatError String

formatErrorTag :: TyCon
formatErrorTag = mkTyCon "AttributeTypes" "FormatError"

instance HasTyCon FormatError where
   tyCon _ = formatErrorTag

formatError :: String -> a
formatError message = throwDyn (FormatError (
   "Attribute format error: "++message))


{- This module handles the basics of representing values, via the
   repository (where files are included). 

   View.hs needs to be compiled before this module, but uses the
   HasCodedValue class.  We allow this by creating an interface
   file CodedValue.hi-boot, which just contain the class definition.
   -}
module CodedValue(
   CodedValue(..),
      -- This represents values in a uniform (and not too inefficient)
      -- way.  A CodedValue is essentially a String, only with record
      -- boundaries.
      -- (actually defined in CodedValueStore)

   safeDecodeIO,
      -- This is recommended instead of the class method decodeIO since in
      -- DEBUG mode it catches format errors and handles them.  In non-DEBUG
      -- it is exactly the same as decodeIO, and should cost nothing more.

   HasCodedValue(..),
      -- class of things that can be converted to and from a CodedValue
      -- Conversion can be impure, and use the repository; hence
      -- (although we won't use it in this file) links and so on will
      -- be included.  The instances of this class form an important
      -- part of this module.  They include integers (of various flavours),
      -- Bool, Char, String.  Also included are Either x y, tuples
      -- of size from 0 to 4 and Maybe x for x already an instance. 
      -- For lists there are two types, [x] and ShortList x,
   ShortList(..),
      -- which are equivalent except that lists are represented with
      -- a record boundary between each element, making them more suitable
      -- where the elements, or the lists, are long enough to make 
      -- CVS-style diff'ing important. 
   List(..),
      -- a synonym for lists.  This is used to evade certain tiresome features
      -- of ghc5.02.1's type system.
   Str(..),
      -- A type Str X where X is an instance of StringClass,
      -- will inherit HasCodedValue according to String.

   -- We also provide the following mechanisms for providing instances
   -- of HasCodedValue:
   
   -- basic manipulation:
   emptyCodedValue,
   isEmptyCodedValue, -- :: CodedValue -> Bool
   addBoundary, -- :: CodedValue -> CodedValue
   removeBoundary, -- :: CodedValue -> Maybe CodedValue

   -- errors on decoding:
   formatError, -- :: String -> a
      -- signals such an error
   catchFormatError, -- :: IO a -> (String -> IO a) -> IO a
      -- catches it.

   -- putting values together.

   -- encoding/decoding one value (without adding it to something before).
   doEncodeIO, 
      -- :: HasCodedValue value => value -> View -> IO CodedValue
   doDecodeIO,
      -- :: HasCodedValue value => CodedValue -> View -> IO value

   -- encoding two values
   encode2IO, -- :: (HasCodedValue value1,HasCodedValue value2) 
      -- => value1 -> value2 -> CodedValue -> View -> IO CodedValue
   decode2IO, -- :: (HasCodedValue value1,HasCodedValue value2)
      -- => CodedValue -> View -> IO ((value1,value2),CodedValue

   -- encoding and decoding multiple values
   -- (This is different from encoding a list directly since we
   -- don't use an accumulating list.)
   -- NB.  These assume that encoding is a 1-1 function.  They should therefore
   -- not be used for something with a null encoding (such as the () type).
   doEncodeMultipleIO, 
      -- :: HasCodedValue value => [value] -> View -> IO CodedValue
   doDecodeMultipleIO,
      -- :: HasCodedValue value => CodedValue -> View -> IO [value]

   HasPureCodedValue(..), -- "pure" coded values, which code without
      -- using IO or the repository.  HasCodedValue follows from this.


   -- HasConverter and the mapEncode*/mapDecode* functions are used
   -- for making an instance of HasCodedValue when we can convert
   -- to/from such an instance.
   HasConverter(..), -- class of conversion functions.
      -- (Nothing follows from this, but it is sometimes useful as an
      -- abbreviation.)

   mapEncodeIO, -- :: HasCodedValue value2 => (value1 -> value2) 
      -- -> (value1 -> CodedValue -> View -> IO CodedValue)
   mapDecodeIO, -- :: HasCodedValue value2 => (value2 -> value1) 
      -- -> (CodedValue -> View -> IO (value1,CodedValue))

   mapEncodePure, -- :: HasPureCodedValue value2 => (value1 -> value2) 
      -- -> (value2 -> CodedValue -> CodedValue)
   mapDecodePure, -- :: HasPureCodedValue value2 => (value2 -> value1)
      -- -> (CodedValue -> (value1,CodedValue))
   ) where

import Char

import Exception
import Dynamics
import Bits
import Int

import UniqueString
import AtomString(StringClass(..))
import Registry
import Sink
import VariableSet
import VariableMap

import VersionDB
import ViewType
import CodedValueStore

---------------------------------------------------------------------
-- CodedValue's and operations on them.
---------------------------------------------------------------------

emptyCodedValue :: CodedValue
emptyCodedValue = CodedValue []

isEmptyCodedValue :: CodedValue -> Bool
isEmptyCodedValue (CodedValue []) = True
isEmptyCodedValue _ = False

addBoundary :: CodedValue -> CodedValue
addBoundary (CodedValue l) = CodedValue (Nothing : l)

removeBoundary :: CodedValue -> Maybe CodedValue
removeBoundary (CodedValue (Nothing : rest)) = Just (CodedValue rest)
removeBoundary _ = Nothing

---
-- prefix combines two codedValues into one codedValue.
-- The format is [length of first codedValue] [codedValue1] [codedValue2]
prefix :: CodedValue -> CodedValue -> CodedValue
prefix (CodedValue l1) (CodedValue l2) =
   let
      len = length l1
   in
      encodePure len (CodedValue (l1 ++ l2))

---
-- unprefix inverts prefix
unprefix :: CodedValue -> (CodedValue,CodedValue) 
unprefix codedValue0 =
   let
      (len,CodedValue l) = decodePure codedValue0
      (l1,l2) = splitAt len l
   in
      (CodedValue l1,CodedValue l2)


---------------------------------------------------------------------
-- HasCodedValue
-- We make this superclass Typeable because in fact we will need
-- Typeable wherever we need HasCodedValue anyway, since View needs
-- to store Versioned items in a dynamic type.
---------------------------------------------------------------------

class Typeable value => HasCodedValue value where
   encodeIO :: value -> CodedValue -> View -> IO CodedValue
      -- prepend a value to a coded value (like show)
   decodeIO :: CodedValue -> View -> IO (value,CodedValue)
      -- extract a value from a coded value (like read)

mapEncodeIO :: HasCodedValue value2 => (value1 -> value2) 
   -> (value1 -> CodedValue -> View -> IO CodedValue)
mapEncodeIO encoder = encodeIO . encoder

mapDecodeIO :: HasCodedValue value2 => (value2 -> value1) 
   -> (CodedValue -> View -> IO (value1,CodedValue))
mapDecodeIO decoder codedValue0 repository =
   do
      (value2,codedValue1) <- safeDecodeIO codedValue0 repository
      return (decoder value2,codedValue1)

encode2IO :: (HasCodedValue value1,HasCodedValue value2) 
   => value1 -> value2 -> CodedValue -> View -> IO CodedValue
encode2IO value1 value2 codedValue0 repository =
   do
      codedValue1 <- encodeIO value2 codedValue0 repository
      codedValue2 <- encodeIO value1 codedValue1 repository
      return codedValue2

decode2IO :: (HasCodedValue value1,HasCodedValue value2)
   => CodedValue -> View -> IO ((value1,value2),CodedValue)
decode2IO codedValue0 repository =
   do
      (value1,codedValue1) <- safeDecodeIO codedValue0 repository
      (value2,codedValue2) <- safeDecodeIO codedValue1 repository
      return ((value1,value2),codedValue2)

doEncodeIO :: HasCodedValue value => value -> View -> IO CodedValue
doEncodeIO value repository = encodeIO value emptyCodedValue repository

doDecodeIO :: HasCodedValue value => CodedValue -> View -> IO value
doDecodeIO codedValue repository =
   wrapFormatError(   
      do
         (value,rest) <- safeDecodeIO codedValue repository
         if isEmptyCodedValue rest
            then
               return value
            else
               formatError "Extra trailing junk"
      )

doEncodeMultipleIO :: HasCodedValue value => [value] -> View -> IO CodedValue
doEncodeMultipleIO values view = doEncodeMultipleIO' values emptyCodedValue
   where
      doEncodeMultipleIO' [] codedValue = return codedValue
      doEncodeMultipleIO' (h:t) codedValue0 =
         do
            codedValue1 <- encodeIO h codedValue0 view
            doEncodeMultipleIO' t codedValue1

doDecodeMultipleIO :: HasCodedValue value => CodedValue -> View -> IO [value]
doDecodeMultipleIO codedValue view = 
      doDecodeMultipleIO' codedValue []
   where
      doDecodeMultipleIO' codedValue0 l =
         if isEmptyCodedValue codedValue0 
         then 
            return l
         else
            do
              (value,codedValue1) <- wrapFormatError(decodeIO codedValue0 view)
              doDecodeMultipleIO' codedValue1 (value:l)

class HasConverter value1 value2 where
   encode' :: value1 -> value2
   decode' :: value2 -> value1

---------------------------------------------------------------------
-- Combining instances of HasCodedValue
---------------------------------------------------------------------

instance (HasCodedValue value1,HasCodedValue value2) 
   => HasCodedValue (value1,value2)
      where
   encodeIO (value1,value2) codedValue repository =
      encode2IO value1 value2 codedValue repository

   decodeIO codedValue repository = decode2IO codedValue repository

instance (HasCodedValue value1,HasCodedValue value2,HasCodedValue value3) 
   => HasCodedValue (value1,value2,value3)
      where
   encodeIO = mapEncodeIO (\ (v1,v2,v3) -> (v1,(v2,v3))) 
   decodeIO = mapDecodeIO (\ (v1,(v2,v3)) -> (v1,v2,v3))

instance (HasCodedValue value1,HasCodedValue value2,HasCodedValue value3,
      HasCodedValue value4) 
   => HasCodedValue (value1,value2,value3,value4)
      where
   encodeIO = mapEncodeIO (\ (v1,v2,v3,v4) -> (v1,(v2,(v3,v4)))) 
   decodeIO = mapDecodeIO (\ (v1,(v2,(v3,v4))) -> (v1,v2,v3,v4))

instance (HasCodedValue value1,HasCodedValue value2,HasCodedValue value3,
      HasCodedValue value4,HasCodedValue value5) 
   => HasCodedValue (value1,value2,value3,value4,value5)
      where
   encodeIO = mapEncodeIO (\ (v1,v2,v3,v4,v5) -> (v1,(v2,(v3,(v4,v5))))) 
   decodeIO = mapDecodeIO (\ (v1,(v2,(v3,(v4,v5)))) -> (v1,v2,v3,v4,v5))



instance (HasCodedValue value1,HasCodedValue value2) 
   => HasCodedValue (Either value1 value2)
      where
   encodeIO (Left value1) codedValue repository =
      encode2IO 'L' value1 codedValue repository
   encodeIO (Right value2) codedValue repository =
      encode2IO 'R' value2 codedValue repository
   decodeIO codedValue0 repository =
      do
         (letter,codedValue1) <- safeDecodeIO codedValue0 repository
         case letter of
            'L' ->
               do
                  (value1,codedValue2) <- safeDecodeIO codedValue1 repository
                  return (Left value1,codedValue2)
            'R' ->
               do
                  (value2,codedValue2) <- safeDecodeIO codedValue1 repository
                  return (Right value2,codedValue2)
            _ -> formatError "Unexpected character decoding Either"

instance HasCodedValue value => HasCodedValue (Maybe value) where
   encodeIO = mapEncodeIO 
      (\ value -> case value of 
         Just value -> Left value
         Nothing -> Right ()
         )
   decodeIO = mapDecodeIO
      (\ value -> case value of
         Left value -> Just value
         Right () -> Nothing
         )

---------------------------------------------------------------------
-- Lists
---------------------------------------------------------------------

---
-- We go via an explicit list constructor.  
newtype List value = List [value]

instance HasCodedValue value => HasCodedValue [value] where
   encodeIO = mapEncodeIO (\ l -> List l)
   decodeIO = mapDecodeIO (\ (List l) -> l)

list_tyRep = mkTyRep "CodedValue" "List"
instance HasTyRep1 List where
   tyRep1 _ = list_tyRep

instance HasCodedValue value => HasCodedValue (List value)
      where
   -- Lists being large items, we put a "Nothing" before
   -- each entry.
   encodeIO (List values) codedValue0 repository =
      do
         let
            l = length values

            encodeValues :: [value] -> CodedValue -> View 
               -> IO CodedValue
            encodeValues [] codedValue repository = return codedValue
            encodeValues (v:vs) codedValue0 repository =
               do
                  codedValue1 <- encodeValues vs codedValue0 repository
                  codedValue2 <- encodeIO v codedValue1 repository
                  let codedValue3 = addBoundary codedValue2
                  return codedValue3

         codedValue1 <- encodeValues values codedValue0 repository
         codedValue2 <- encodeIO l codedValue1 repository
         return codedValue2
   decodeIO codedValue0 repository =
      do
         (l :: Int,codedValue1) <- safeDecodeIO codedValue0 repository
         let
            decodeNValues :: Int -> CodedValue -> IO ([value],CodedValue)
            decodeNValues n codedValue0 =
               if n<=0
                  then
                     if n<0
                        then 
                           formatError 
                              "Bad list length in CodedValue.decodeIO"
                        else
                           return ([],codedValue0)
                  else
                     do
                        codedValue1 <-
                           case removeBoundary codedValue0 of
                              Just codedValue -> return codedValue
                              Nothing -> 
                                 formatError "List element boundary missing"
                        (value1,codedValue2) 
                           <- safeDecodeIO codedValue1 repository
                        (values,codedValue3) <- decodeNValues (n-1) codedValue2
                        return (value1:values,codedValue3)
         (list,codedValue) <- decodeNValues l codedValue1
         return (List list,codedValue)

---------------------------------------------------------------------
-- ShortLists
-- are like lists, except we don't put a record boundary
-- at the start of each item (and so they are intended for 
-- Strings etcetera).
---------------------------------------------------------------------

newtype ShortList a = ShortList [a]

-- make them Typeable
shortList_tyRep = mkTyRep "CodedValue" "ShortList"

instance HasTyRep1 ShortList where
   tyRep1 _ = shortList_tyRep

instance HasCodedValue value => HasCodedValue (ShortList value)
      where
   encodeIO (ShortList values) codedValue0 repository =
      do
         let
            l = length values

            encodeValues :: [value] -> CodedValue -> View 
               -> IO CodedValue
            encodeValues [] codedValue repository = return codedValue
            encodeValues (v:vs) codedValue0 repository =
               do
                  codedValue1 <- encodeValues vs codedValue0 repository
                  codedValue2 <- encodeIO v codedValue1 repository
                  return codedValue2

         codedValue1 <- encodeValues values codedValue0 repository
         codedValue2 <- encodeIO l codedValue1 repository
         return codedValue2
   decodeIO codedValue0 repository =
      do
         (l :: Int,codedValue1) <- safeDecodeIO codedValue0 repository
         let
            decodeNValues :: Int -> CodedValue -> IO ([value],CodedValue)
            decodeNValues n codedValue0 =
               if n<=0
                  then
                     if n<0
                        then 
                           formatError 
                              "Bad list length in CodedValue.decodeIO"
                        else
                           return ([],codedValue0)
                  else
                     do
                        (value1,codedValue1) 
                           <- safeDecodeIO codedValue0 repository
                        (values,codedValue2) <- decodeNValues (n-1) codedValue1
                        return (value1:values,codedValue2)
         (list,codedValue2) <- decodeNValues l codedValue1
         return (ShortList list,codedValue2)

---------------------------------------------------------------------
-- Pure Coded Values (which don't require IO or access to the repository)
---------------------------------------------------------------------

class Typeable value => HasPureCodedValue value where
    encodePure :: value -> CodedValue -> CodedValue
    decodePure :: CodedValue -> (value,CodedValue)

instance HasPureCodedValue value => HasCodedValue value where
    encodeIO value codedValue repository =
       return (encodePure value codedValue)
    decodeIO codedValue repository =
       -- force an error to be raised here if there is one.
       codedValue `seq` (return (decodePure codedValue))

mapEncodePure :: HasPureCodedValue value2 => (value1 -> value2) 
   -> (value1 -> CodedValue -> CodedValue)
mapEncodePure encoder = encodePure . encoder

mapDecodePure :: HasPureCodedValue value2 => (value2 -> value1)
   -> (CodedValue -> (value1,CodedValue))
mapDecodePure decoder codedValue0 =
   let
      (value2,codedValue1) = decodePure codedValue0
   in
      (decoder value2,codedValue1)

---------------------------------------------------------------------
-- Basic Types apart from integers
---------------------------------------------------------------------


instance HasPureCodedValue () where
   -- used in fact for encoding Maybe.
   encodePure () codedValue = codedValue
   decodePure codedValue = ((),codedValue)

instance HasPureCodedValue Char where
   encodePure ch (CodedValue soFar) = CodedValue ((Just ch) : soFar)
   decodePure (CodedValue (Just ch : rest)) = (ch,CodedValue rest)
   decodePure _ = formatError "Decoding char, found an unexpected record end"

-- String's.
-- The following overlapping instance explicitly makes Strings go via
-- ShortList Char.
instance HasCodedValue String where
   encodeIO = mapEncodeIO (\ str -> ShortList str)
   decodeIO = mapDecodeIO (\ (ShortList str) -> str)

-- We automate StringClass => HasCodedValue, but wrap it
-- up in the Str type to allow alternative instances.
newtype Str a = Str a

str_tyRep = mkTyRep "CodedValue" "Str"
instance HasTyRep1 Str where
   tyRep1 _ = str_tyRep 

instance (Typeable str,StringClass str) => HasCodedValue (Str str) where
   encodeIO = mapEncodeIO (\ (Str str) -> toString str)
   decodeIO = mapDecodeIO (\ str -> Str (fromString str))

-- Bool's.
instance HasPureCodedValue Bool where
   encodePure = mapEncodePure (\ b -> if b then 'T' else 'F')
   decodePure = mapDecodePure (\ c -> case c of
      'T' -> True
      'F' -> False
      ch -> formatError ("Decoding bool, found an unexpected char "
         ++show ch)
      )

-- We make CodedValue an instance itself

codedValue_tyRep = mkTyRep "CodedValue" "CodedValue"
instance HasTyRep CodedValue where
   tyRep _ = codedValue_tyRep

instance HasPureCodedValue CodedValue where
   encodePure value codedValue = prefix value codedValue
   decodePure codedValue = unprefix codedValue

---------------------------------------------------------------------
-- UniqueStringSource
---------------------------------------------------------------------

instance HasCodedValue UniqueStringSource where
   encodeIO v codedValue view =
      do
         l <- readUniqueStringSource v
         encodeIO l codedValue view
   decodeIO codedValue0 view =
      do
         (l,codedValue1) <- safeDecodeIO codedValue0 view
         v <- createUniqueStringSource l
         return (v,codedValue1)

---------------------------------------------------------------------
-- Location and ObjectVersion (from CVSDB)
---------------------------------------------------------------------

instance HasCodedValue Location where
   encodeIO = mapEncodeIO (\ location -> Str location)
   decodeIO = mapDecodeIO (\ (Str location) -> location)

instance HasCodedValue ObjectVersion where
   encodeIO = mapEncodeIO (\ objectVersion -> Str objectVersion)
   decodeIO = mapDecodeIO (\ (Str objectVersion) -> objectVersion)

---------------------------------------------------------------------
-- Integers
---------------------------------------------------------------------

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

codedList_tyRep = mkTyRep "CodedValue" "CodedList"
instance HasTyRep CodedList where
   tyRep _ = codedList_tyRep

chrGeneral :: Integral a => a -> Char
chrGeneral value = chr (fromIntegral value)

ordGeneral :: Integral a => Char -> a
ordGeneral value = fromIntegral (ord value)

instance HasPureCodedValue CodedList where
   encodePure (CodedList [i]) codedValue =
      encodePure (chrGeneral (i .|. topBit)) codedValue
   encodePure (CodedList (x:xs)) codedValue =
      encodePure (chrGeneral x) (encodePure (CodedList xs) codedValue)

   decodePure codedValue =
      let
         (ch,nextCodedValue) = decodePure codedValue 
         i = ordGeneral ch
      in
         if i<topBit
            then
               let
                  (CodedList xs,rest) = decodePure nextCodedValue
               in
                  (CodedList (i:xs),rest)
            else
               (CodedList [i `xor` topBit],nextCodedValue)

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


instance (Typeable integral,Integral integral,Bits integral) 
   => HasPureCodedValue integral where
   encodePure value codedValue = 
      encodePure (encode' value :: CodedList) codedValue
   decodePure codedValue0 =
      let
          (value' :: CodedList,codedValue1) = decodePure codedValue0
      in
          (decode' value',codedValue1)

---------------------------------------------------------------------
-- We make Registry's an instance of HasCodedValue
---------------------------------------------------------------------

instance (HasCodedValue from,HasCodedValue to,Ord from) 
      => HasCodedValue (Registry from to) where
   encodeIO value codedValue view =
      do
         contents <- listRegistryContents value
         encodeIO value codedValue view
   decodeIO codedValue0 view =
      do
         (contents,codedValue1) <- safeDecodeIO codedValue0 view
         registry <- listToNewRegistry contents
         return (registry,codedValue1)

---------------------------------------------------------------------
-- We make VariableSet's and VariableMap's an instance of HasCodedValue
---------------------------------------------------------------------

instance (Typeable x,HasCodedValue [x],HasKey x key) 
      => HasCodedValue (VariableSet x) where
   encodeIO variableSet codedValue view =
      do
         list <- readContents variableSet
         encodeIO list codedValue view

   decodeIO codedValue0 view =
      do
         (list,codedValue1) <- safeDecodeIO codedValue0 view
         variableSet <- newVariableSet list
         return (variableSet,codedValue1)

instance (Ord key,HasCodedValue key,HasCodedValue elt) 
   => HasCodedValue (VariableMap key elt) where

   encodeIO variableMap codedValue view =
      do
         mapData <- readContents variableMap
         let list = mapToList mapData
         encodeIO list codedValue view

   decodeIO codedValue0 view =
      do
         (list,codedValue1) <- safeDecodeIO codedValue0 view
         variableMap <- newVariableMap list
         return (variableMap,codedValue1)
         

---------------------------------------------------------------------
-- FormatError's
---------------------------------------------------------------------

newtype FormatError = FormatError String

formatErrorTag :: TyRep
formatErrorTag = mkTyRep "CodedValue" "FormatError"

instance HasTyRep FormatError where
   tyRep _ = formatErrorTag

formatError :: String -> a
formatError message = throwDyn (FormatError (
   "format error: "++message))

catchFormatError :: IO a -> (String -> IO a) -> IO a
catchFormatError =
   catchJust
      (\ exception -> -- get out the FormatError String, if
                      -- it's here
         case dynExceptions exception of
            Just dyn ->
               case fromDyn dyn of
                  Just (FormatError str) -> Just str
                  Nothing -> Nothing
            Nothing -> Nothing
         )

-- -------------------------------------------------------------------
-- Functions for tracking down format errors in input.
-- -------------------------------------------------------------------

---
-- Used in this file to protect functions which may raise a format error
-- on reading something.  wrapFormatError should be put round functions 
-- used at the interface to the user.
wrapFormatError :: HasCodedValue a => IO a -> IO a
#ifdef DEBUG
wrapFormatError (action :: IO a) =
   catchFormatError action
      (\ str ->
         let
            tyPar = undefined :: a
         in 
            error ("CodedValue: "++str++" reading value of type "++
               show (typeOf tyPar))
         )
#else 
wrapFormatError action = action
{-# INLINE wrapFormatError #-}
#endif

---
-- Used in this file instead of decodeIO.
safeDecodeIO :: HasCodedValue value 
   => CodedValue -> View -> IO (value,CodedValue)
#ifdef DEBUG
safeDecodeIO codedValue view =
      wrapFormatError2 (decodeIO codedValue view)
   where
      wrapFormatError2 (action :: IO (a,CodedValue)) =
         catchFormatError action
            (\ str ->
               let
                  tyPar = undefined :: a
                  CodedValue l = codedValue
               in 
                  formatError ("CodedValue: "++str++" reading value of type "++
                     show (typeOf tyPar) ++ " from "++show (take 10 l)++"...")
               )

#else
safeDecodeIO = decodeIO
#endif
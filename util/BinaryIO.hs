{- This module contains various code for writing and reading things to and
   from a Handle efficiently.  In particular CStringLens.  -}
module BinaryIO(
   HasBinaryIO(..), -- class of things that can be encoded.
      -- instances defined: 
      --    ICStringLen,
      --    integers,
      --    Strings,
      --    Bool,
      --    tuples and lists,
      --    Choice5, 
      --    ReadShow a where a instances Read and Show
      --    Str a where a instances StringClass
   ReadShow(..),
   Str(..),

   -- We define 
   Choice5(..),
      -- A choice between five types

   -- HasWrapper is used as a way of constructing HasBinaryIO instances
   -- for general datatypes.
   HasWrapper(..),
   Wrap(..),UnWrap(..),
   wrap0,wrap1,wrap2,wrap3,
      -- used for constructing HasWrapper instances.

   HasConverter(..), -- class for converting to and from binary representations
   CodedList(..), -- binary representation (basically a list of integers)
   topBit, -- top bit of characters, as an integer.
   chrGeneral, -- functions for converting integers to and from Char.
   ordGeneral,


   -- mapHPut and mapHGetIntWE are used for constructing instances of
   -- HasBinaryIO, given functions which convert them to and from other 
   -- instances.  (For examples, see the sourcecode of this module.)
   mapHPut,
      -- :: HasBinaryIO value2 => (value1 -> value2) -> Handle -> value1 
      -- -> IO ()
   mapHGetIntWE,
      -- :: HasBinaryIO value2 => (value2 -> value1) -> Int -> Handle
      -- -> IO (WithError value1)
   ) where

import IO hiding (hGetChar)
import qualified HGetCharHack (hackhGetChar)

import Char

import Data.Bits
import Data.Word
import GHC.Int(Int32)
import GHC.IO hiding (hGetChar)
import System.IO.Error
import Control.Exception

import Computation
import ExtendedPrelude
import ICStringLen
import AtomString

hGetChar = HGetCharHack.hackhGetChar

-- ---------------------------------------------------------------------------
-- The HasBinaryIO class
-- ---------------------------------------------------------------------------

class HasBinaryIO value where
   hPut :: Handle -> value -> IO ()
   hGet :: Handle -> IO value
   hGetWE :: Handle -> IO (WithError value)
      -- this catches parsing errors (but not IO errors).

   hGetInt :: Int -> Handle -> IO value
   hGetIntWE :: Int -> Handle -> IO (WithError value)
      -- this allows an extra integer argument to be passed to the parsing
      -- function, which is useful in some cases. 

      -- minimally either hGetWE or hGetIntWE should be defined.

   hGet handle = 
      do
         value <- hGetWE handle
         coerceWithErrorIO value

   hGetInt i handle = 
      do
         value <- hGetIntWE i handle
         coerceWithErrorIO value

   hGetWE = hGetIntWE 4

   hGetIntWE _ = hGetWE

-- ---------------------------------------------------------------------------
-- Instances of Read/Show
-- ---------------------------------------------------------------------------

newtype ReadShow a = ReadShow a

instance (Read a,Show a) => HasBinaryIO (ReadShow a) where
   hPut handle (ReadShow value) = hPut handle (show value)
   hGetWE handle =
      do
         strWE <- hGetWE handle
         return (mapWithError'
            (\ str -> case readsPrec 0 str of
               [(value,"")] -> hasValue (ReadShow value)
               [(value,extra)] -> hasError (
                  "Extra characters parsing " ++ show str)
               _ -> hasError ("Couldn't parse " ++ show str)
               )
            strWE
            )
         


-- ---------------------------------------------------------------------------
-- Instances of StringClass
-- ---------------------------------------------------------------------------

newtype Str a = Str a

instance StringClass a => HasBinaryIO (Str a) where
   hPut handle (Str value) = hPut handle (toString value)
   hGetIntWE limit handle =
      do
         (strWE :: WithError String) <- hGetIntWE limit handle 
         return (mapWithError
            (Str . fromString)
            strWE
            )

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------

instance HasBinaryIO Char where
   hPut = mapHPut ord
   hGetIntWE = mapHGetIntWE chr

-- ---------------------------------------------------------------------------
-- String
-- ---------------------------------------------------------------------------

instance HasBinaryIO String where
   hPut handle str =
      do
         let
            icsl :: ICStringLen
            icsl = fromString str

         hPut handle icsl
   hGetIntWE limit handle =
      do
         (icslWE :: WithError ICStringLen) <- hGetIntWE limit handle
         return (mapWithError toString icslWE)

-- ---------------------------------------------------------------------------
-- ICStringLen
-- ---------------------------------------------------------------------------

instance HasBinaryIO ICStringLen where
   hPut handle icsl =
      withICStringLen icsl
         (\ len cString -> 
            do
               hPut handle len
               hPutBuf handle cString len
            )

   hGetIntWE limit handle =
      do
         (lenWE :: WithError Int) <- hGetIntWE limit handle
         mapWithErrorIO'
            (\ len ->
               do
                  if len < 0
                     then
                        return (hasError "Negative string length")
                     else
                        do
                           (icsl,unitWE) <- mkICStringLenExtra len 
                              (\ cString ->
                                  do
                                     lenRead <- hGetBuf handle cString len
                                     if lenRead < len
                                        then
                                           let
                                              eofError =
                                                 mkIOError eofErrorType
                                                    "BinaryIO" (Just handle)
                                                    Nothing
                                           in
                                              throw eofError
                                        else
                                           return (hasValue ()) 
                                  )
                           return (mapWithError (\ () -> icsl) unitWE)
               )
            lenWE

   hGetWE = hGetIntWE 4 -- allow a higher default


-- ---------------------------------------------------------------------------
-- CodedList's.  These are used indirectly for integers.
-- ---------------------------------------------------------------------------

newtype CodedList = CodedList [Int32] 
-- This is a nonempty list of integers in [0,2^(bitsInChar-1)).

instance HasBinaryIO CodedList where
   -- we set the top bit of the last list item.
   hPut handle (CodedList [i]) =
      hPutChar handle (chrGeneral (i .|. topBit))
   hPut handle (CodedList (i:is)) =
      do
         hPutChar handle (chrGeneral i)
         hPut handle (CodedList is)

   -- the integer is a bound on the maximum length
   hGetIntWE limit handle =
      if limit <= 0
         then
            return (hasError "Overflow trying to read integer value")
         else
            do
               ch <- hGetChar handle
               let
                  i = ordGeneral ch
               if i >= topBit
                  then
                     return (hasValue (CodedList [i `xor` topBit]))
                  else
                     do
                        clWE <- hGetIntWE (limit-1) handle
                        return (mapWithError 
                           (\ (CodedList is) -> 
                              CodedList (i : is)
                              )
                           clWE
                           )

chrGeneral :: Integral a => a -> Char
chrGeneral value = chr (fromIntegral value)

ordGeneral :: Integral a => Char -> a
ordGeneral value = fromIntegral (ord value)

-- ---------------------------------------------------------------------------
-- Integers
-- We do the most common integral types, but not 
-- ---------------------------------------------------------------------------

instance HasBinaryIO Int where
   hPut = mapHPut encodeIntegral
   hGetIntWE = mapHGetIntWE decodeIntegral

instance HasBinaryIO Word where
   hPut = mapHPut encodeIntegral
   hGetIntWE = mapHGetIntWE decodeIntegral

instance HasBinaryIO Int32 where
   hPut = mapHPut encodeIntegral
   hGetIntWE = mapHGetIntWE decodeIntegral

instance HasBinaryIO Word32 where
   hPut = mapHPut encodeIntegral
   hGetIntWE = mapHGetIntWE decodeIntegral

instance HasBinaryIO Integer where
   hPut = mapHPut encodeIntegral
   hGetIntWE = mapHGetIntWE decodeIntegral

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

class HasConverter value1 value2 where
   encode' :: value1 -> value2
   decode' :: value2 -> value1

encodeIntegral :: (Integral integral,Bits integral) 
   => integral -> CodedList
encodeIntegral = encode'

decodeIntegral :: (Integral integral,Bits integral) 
   => CodedList -> integral
decodeIntegral = decode'

instance (Integral integral,Bits integral) 
   => HasConverter integral CodedList 
      where
   encode' i =
      if isLarge i
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
      where
         isLarge :: integral -> Bool
         -- we need separate versions for unsigned and signed types
         isLarge =
            if ( -nextBit :: integral) < 0
               then -- integral
                  (\ i -> (i >= nextBit) || (i < -nextBit))
               else -- word
                  (\ i -> i >= nextBit)
 
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


-- ---------------------------------------------------------------------------
--
-- ** Ways of combining instances of HasBinaryIO ******
-- 
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Unit.
-- ---------------------------------------------------------------------------

instance HasBinaryIO () where
   hPut handle () = done
   hGetWE handle = return (hasValue ())


-- ---------------------------------------------------------------------------
-- When we can convert to something else we can encode.
-- ---------------------------------------------------------------------------

mapHPut :: HasBinaryIO value2 => (value1 -> value2) -> Handle -> value1 
   -> IO ()
mapHPut mapFn handle value1 = hPut handle (mapFn value1)

mapHGetIntWE :: HasBinaryIO value2 => (value2 -> value1) -> Int -> Handle
   -> IO (WithError value1)
mapHGetIntWE mapFn limit handle =
   do
      value2WE <- hGetIntWE limit handle
      return (mapWithError mapFn value2WE)

-- ---------------------------------------------------------------------------
-- Tuples
-- ---------------------------------------------------------------------------

instance (HasBinaryIO value1,HasBinaryIO value2) 
   => HasBinaryIO (value1,value2) where

   hPut handle (value1,value2) =
      do
         hPut handle value1
         hPut handle value2

   hGetIntWE limit handle =
      do
         value1WE <- hGetIntWE limit handle 
         mapWithErrorIO'
            (\ value1 ->
               do
                  value2WE <- hGetIntWE limit handle
                  return (mapWithError (\ value2 -> (value1,value2)) value2WE)
               )
            value1WE

instance (HasBinaryIO value1,HasBinaryIO (value2,value3)) 
    => HasBinaryIO (value1,value2,value3) where

    hPut = mapHPut (\ (value1,value2,value3) -> (value1,(value2,value3)))
    hGetIntWE = mapHGetIntWE (\ (value1,(value2,value3))
       -> (value1,value2,value3))

-- ---------------------------------------------------------------------------
-- Bool
-- ---------------------------------------------------------------------------

encodeBool :: Bool -> Either () ()
encodeBool = encode'

decodeBool :: Either () () -> Bool 
decodeBool = decode'

instance HasConverter Bool (Either () ()) where
   encode' False = Left ()
   encode' True = Right ()
   decode' (Left ()) = False
   decode' (Right ()) = True

instance HasBinaryIO Bool where
    hPut = mapHPut encodeBool
    hGetIntWE = mapHGetIntWE decodeBool

   
-- ---------------------------------------------------------------------------
-- Maybe
-- ---------------------------------------------------------------------------

encodeMaybe :: Maybe v -> Either v ()
encodeMaybe = encode'

decodeMaybe :: Either v () -> Maybe v 
decodeMaybe = decode'

instance HasConverter (Maybe v) (Either v ()) where
   encode' (Just v) = Left v
   encode' Nothing = Right ()
   decode' (Left v) = Just v
   decode' (Right ()) = Nothing

instance HasBinaryIO v => HasBinaryIO (Maybe v) where
    hPut = mapHPut encodeMaybe
    hGetIntWE = mapHGetIntWE decodeMaybe

   
-- ---------------------------------------------------------------------------
-- Either
-- ---------------------------------------------------------------------------

instance (HasBinaryIO v1,HasBinaryIO v2) => HasBinaryIO (Either v1 v2) where
   hPut handle (Left v1) =
      do
         hPutChar handle 'L'
         hPut handle v1
   hPut handle (Right v2) =
      do
         hPutChar handle 'R'
         hPut handle v2
   hGetIntWE limit handle =
      do
         switch <- hGetChar handle
         let
            ret cons vWE = return (mapWithError cons vWE)
         case switch of
            'L' ->
               do
                  v1WE <- hGetIntWE limit handle 
                  ret Left v1WE
            'R' ->
               do
                  v2WE <- hGetIntWE limit handle 
                  ret Right v2WE
            _ -> return (hasError ("BinaryIO.Either - unexpected " 
               ++ [switch]))
               


-- ---------------------------------------------------------------------------
-- Choice of 5 (or up to 5) types.
-- ---------------------------------------------------------------------------


---
-- When you want to encode larger choices, Choice5 is recommended.
-- If the choice is between fewer than 5 items, just set the unused
-- type variables to ().
data Choice5 v1 v2 v3 v4 v5 =
      Choice1 v1
   |  Choice2 v2
   |  Choice3 v3
   |  Choice4 v4
   |  Choice5 v5

instance 
   (HasBinaryIO v1,HasBinaryIO v2,HasBinaryIO v3,HasBinaryIO v4,HasBinaryIO v5)
   => HasBinaryIO (Choice5 v1 v2 v3 v4 v5) where

   hPut handle (Choice1 v1) =
      do
         hPutChar handle '1'
         hPut handle v1
   hPut handle (Choice2 v2) =
      do
         hPutChar handle '2'
         hPut handle v2
   hPut handle (Choice3 v3) =
      do
         hPutChar handle '3'
         hPut handle v3
   hPut handle (Choice4 v4) =
      do
         hPutChar handle '4'
         hPut handle v4
   hPut handle (Choice5 v5) =
      do
         hPutChar handle '5'
         hPut handle v5

   hGetIntWE limit handle =
      do
         switch <- hGetChar handle
         let
            ret cons vWE = return (mapWithError cons vWE)
          
         case switch of
            '1' ->
               do
                  v1WE <- hGetIntWE limit handle 
                  ret Choice1 v1WE
            '2' ->
               do
                  v2WE <- hGetIntWE limit handle 
                  ret Choice2 v2WE
            '3' ->
               do
                  v3WE <- hGetIntWE limit handle 
                  ret Choice3 v3WE
            '4' ->
               do
                  v4WE <- hGetIntWE limit handle 
                  ret Choice4 v4WE
            '5' ->
               do
                  v5WE <- hGetIntWE limit handle 
                  ret Choice5 v5WE
            _ -> return (hasError ("BinaryIO.Choice5 - unexpected " 
               ++ [switch]))
  
-- ---------------------------------------------------------------------------
-- Lists (apart from Strings)
-- ---------------------------------------------------------------------------

instance HasBinaryIO value => HasBinaryIO [value] where

   hPut handle list =
      do
         let
            len = length list
         hPut handle len
         mapM_ (hPut handle) list

   hGetIntWE limit handle =
      do
         lenWE <- hGetIntWE limit handle
         let
            readN :: Int -> IO (WithError [value])
            readN 0 = return (hasValue [])
            readN n =
               do
                  valueWE <- hGetIntWE limit handle
                  mapWithErrorIO' 
                     (\ value ->
                        do
                           valuesWE <- readN (n-1)
                           return (mapWithError
                              (\ values -> value : values)
                              valuesWE
                              )
                        )
                     valueWE

         mapWithErrorIO'
            (\ len ->
               if len < 0
                  then
                     return (hasError "Negative list length")
                  else
                     readN len
               )
            lenWE 


-- ---------------------------------------------------------------------------
-- More complex structures.
-- This is used as a general extensible way of encoding more complex datatypes.
-- ---------------------------------------------------------------------------

class HasWrapper wrapper where
   wraps :: [Wrap wrapper] 
      -- How to construct wrapper type.  Argument gives a list of alternatives.
   unWrap :: wrapper -> UnWrap
      -- How to deconstruct.

-- Blame GHC that we can't use labels with existential types.
data UnWrap = forall val . HasBinaryIO val
   => UnWrap 
      Char -- label for this type on writing.
      val -- value inside this wrapped type.

data Wrap wrapper = forall val . HasBinaryIO val
   => Wrap
      Char -- label for this type on reading.  This must, of course, be the
           -- same as for the corresponding UnWrap.
      (val -> wrapper)
           -- how to wrap this sort of value.

-- some abbreviations for construtor functions with varying numbers of 
-- arguments
wrap0 :: Char -> wrapper -> Wrap wrapper
wrap0 label wrapper = Wrap label (\ () -> wrapper)

wrap1 :: HasBinaryIO val => Char -> (val -> wrapper) -> Wrap wrapper
wrap1 = Wrap

wrap2 :: (HasBinaryIO val1,HasBinaryIO val2) => Char 
   -> (val1 -> val2 -> wrapper) -> Wrap wrapper
wrap2 char con = Wrap char (\ (val1,val2) -> con val1 val2)

wrap3 :: (HasBinaryIO val1,HasBinaryIO val2,HasBinaryIO val3) => Char 
   -> (val1 -> val2 -> val3 -> wrapper) -> Wrap wrapper
wrap3 char con = Wrap char (\ (val1,val2,val3) -> con val1 val2 val3)

instance HasWrapper wrapper => HasBinaryIO wrapper where
   hPut handle wrapper = hPut' (unWrap wrapper)
      where
         hPut' (UnWrap label val) =
            do
               hPutChar handle label
               hPut handle val
   hGetIntWE limit handle =
      do
         thisLabel <- hGetChar handle
         let
            innerWrap :: HasBinaryIO val 
               => (val -> wrapper) -> IO (WithError wrapper) 
            innerWrap wrapFn =
               do
                  valWE <- hGetIntWE limit handle
                  return (mapWithError wrapFn valWE)

         case findJust
            (\ (Wrap label wrapFn) -> 
               if label == thisLabel then Just (innerWrap wrapFn) else Nothing
               )
            wraps of

            Nothing -> return (hasError ("BinaryIO.HasWrapper - bad label "
               ++ show thisLabel))
            Just getWrap -> getWrap

{- Here is a little example -}
data Tree val =
      Leaf val
   |  Node [Tree val]

instance HasBinaryIO val => HasWrapper (Tree val) where
   wraps = [
      Wrap 'L' Leaf,
      Wrap 'N' Node
      ]
   unWrap = (\ wrapper -> case wrapper of
      Leaf v -> UnWrap 'L' v
      Node l -> UnWrap 'N' l
      )


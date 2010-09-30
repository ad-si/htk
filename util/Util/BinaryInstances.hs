{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Instances of the 'Binary.HasBinary' class.  This includes the
-- standard types (except of course for things like function types and
-- IO) plus a few others.
module Util.BinaryInstances(
   -- Methods provided for encoding alternatives
   Choice5(..),
      -- 5-way alternatives.

   HasWrapper(..), -- class for unlimited (well, up to 256) alternatives.
      -- instance this class and you get an instance of HasBinary
   Wrapped(..),
   UnWrap(..),
   wrap0,wrap1,wrap2,wrap3,wrap4,
      -- used for instancing.

   ReadShow(..),
      -- A wrapper for things which are to be represented by their
      -- Read/Show instances.
   ViaEnum(..),
      -- A wrapper for things which are to be represented by their
      -- Enum instances.

   Unsigned(..),
      -- A wrapper for unsigned integral types.
   ) where

import Data.Char

-- GHC modules
import Data.Bits
import Data.Word
import GHC.Int(Int32)
import Foreign.C.Types

-- Our modules
import Util.Bytes
import Util.Binary
import Util.BinaryUtils

-- -----------------------------------------------------------------------
-- Encoding tuples (we go up to 5).
-- -----------------------------------------------------------------------

instance Monad m => HasBinary () m where
   writeBin wb () = return ()
   readBin rb = return ()

instance (Monad m,HasBinary v1 m,HasBinary v2 m) => HasBinary (v1,v2) m where
   writeBin wb (v1,v2) =
      do
         writeBin wb v1
         writeBin wb v2
   readBin wb =
      do
         v1 <- readBin wb
         v2 <- readBin wb
         return (v1,v2)

instance (Monad m,HasBinary v1 m,HasBinary (v2,v3) m)
   => HasBinary (v1,v2,v3) m where
   writeBin = mapWrite (\ (v1,v2,v3) -> (v1,(v2,v3)))
   readBin = mapRead (\ (v1,(v2,v3)) -> (v1,v2,v3))

instance (Monad m,HasBinary v1 m,HasBinary (v2,v3,v4) m)
   => HasBinary (v1,v2,v3,v4) m where
   writeBin = mapWrite (\ (v1,v2,v3,v4) -> (v1,(v2,v3,v4)))
   readBin = mapRead (\ (v1,(v2,v3,v4)) -> (v1,v2,v3,v4))

instance (Monad m,HasBinary v1 m,HasBinary (v2,v3,v4,v5) m)
   => HasBinary (v1,v2,v3,v4,v5) m where
   writeBin = mapWrite (\ (v1,v2,v3,v4,v5) -> (v1,(v2,v3,v4,v5)))
   readBin = mapRead (\ (v1,(v2,v3,v4,v5)) -> (v1,v2,v3,v4,v5))

instance (Monad m,HasBinary v1 m,HasBinary (v2,v3,v4,v5,v6) m)
   => HasBinary (v1,v2,v3,v4,v5,v6) m where
   writeBin = mapWrite (\ (v1,v2,v3,v4,v5,v6) -> (v1,(v2,v3,v4,v5,v6)))
   readBin = mapRead (\ (v1,(v2,v3,v4,v5,v6)) -> (v1,v2,v3,v4,v5,v6))


instance (Monad m,HasBinary v1 m,HasBinary (v2,v3,v4,v5,v6,v7) m)
   => HasBinary (v1,v2,v3,v4,v5,v6,v7) m where
   writeBin = mapWrite (\ (v1,v2,v3,v4,v5,v6,v7) -> (v1,(v2,v3,v4,v5,v6,v7)))
   readBin = mapRead (\ (v1,(v2,v3,v4,v5,v6,v7)) -> (v1,v2,v3,v4,v5,v6,v7))

-- -----------------------------------------------------------------------
-- Encoding Byte and (Bytes,Int).
-- NB.  We assume that the (Int) is non-negative!!!
-- -----------------------------------------------------------------------

instance HasBinary Byte m where
   writeBin wb byte = writeByte wb byte
   readBin wb = readByte wb

instance Monad m => HasBinary (Bytes,Int) m where
   writeBin wb (bytes,len) =
      do
         writeBin wb ( (fromIntegral len) :: Word)
         writeBytes wb bytes len
   readBin wb =
      do
         (lenW :: Word) <- readBin wb
         let
            len = fromIntegral lenW
         bytes <- readBytes wb len
         return (bytes,len)

-- -----------------------------------------------------------------------
-- Encoding Either/Maybe/Bool
-- -----------------------------------------------------------------------

instance (Monad m,HasBinary a m) => HasBinary (Maybe a) m where
   writeBin = mapWrite (\ aOpt -> case aOpt of
      Nothing -> Left ()
      Just a -> Right a
      )
   readBin = mapRead (\ aEither -> case aEither of
      Left () -> Nothing
      Right a -> Just a
     )

instance (Monad m,HasBinary a m,HasBinary b m)
   => HasBinary (Either a b) m where

   writeBin wb (Left a) =
      do
         writeBin wb False
         writeBin wb a
   writeBin wb (Right b) =
      do
         writeBin wb True
         writeBin wb b
   readBin rb =
      do
         isRight <- readBin rb
         if isRight
            then
               do
                  b <- readBin rb
                  return (Right b)
            else
               do
                  a <- readBin rb
                  return (Left a)


instance Monad m => HasBinary Bool m where
   writeBin = mapWrite (\ b -> if b then (1 :: Byte) else 0)
   readBin rb =
      do
         (switch :: Byte) <- readBin rb
         case switch of
            0 -> return False
            1 -> return True
            _ -> fail ("BinaryInstances.Bool - unexpected switch "
               ++ show switch)


-- -----------------------------------------------------------------------
-- Encoding Char (yes, we do Unicode, although this costs us)
-- -----------------------------------------------------------------------

instance Monad m => HasBinary Char m where
   writeBin = mapWrite (\ c -> (fromIntegral . ord $ c) :: Word)
   readBin = mapRead (\ (w :: Word) -> chr . fromIntegral $ w)

-- -----------------------------------------------------------------------
-- Encoding lists
-- -----------------------------------------------------------------------

instance (Monad m,HasBinary a m) => HasBinary [a] m where
   writeBin wb as =
      do
         writeBin wb (fromIntegral (length as) :: Word)
         mapM_ (\ a -> writeBin wb a) as
   readBin wb =
      do
         (len :: Word)<- readBin wb
         as <- mapM (\ _ -> readBin wb) [1..len]
         return as


-- -----------------------------------------------------------------------
-- Encoding integers
-- Some features of our encoding.
-- (1) integers have the same encoding and words have the same encoding,
--     however the two encodings differ slightly, since words don't have
--     to store the sign.  This is important since it means ASCII characters
--     can be stored in one byte (they go via word).
-- (1) it is independent of the sort of integer in question.
-- (2) it is variable size, so that small integers (which are rather common)
-- fit into one byte.
-- -----------------------------------------------------------------------

instance Monad m => HasBinary Int m where
   writeBin = mapWrite encodeIntegral
   readBin = mapRead decodeIntegral

instance Monad m => HasBinary Word m where
   writeBin = mapWrite encodeWord
   readBin = mapRead decodeWord

instance Monad m => HasBinary Int32 m where
   writeBin = mapWrite encodeIntegral
   readBin = mapRead decodeIntegral

instance Monad m => HasBinary Word32 m where
   writeBin = mapWrite encodeWord
   readBin = mapRead decodeWord

instance Monad m => HasBinary Integer m where
   writeBin = mapWrite encodeIntegral
   readBin = mapRead decodeIntegral

instance Monad m => HasBinary CSize m where
   writeBin = mapWrite encodeWord
   readBin = mapRead decodeWord

encodeIntegral :: (Integral integral,Bits integral) => integral -> CodedList
encodeIntegral (i :: integral) =
   if isLarge i
      then
         let
            lowestPart = i .&. mask
            highPart = i `shiftR` bitsPerByte
            CodedList codedHigh = encodeIntegral highPart
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
      isLarge = (\ i -> (i >= nextBit) || (i < -nextBit))


decodeIntegral :: (Integral integral,Bits integral) => CodedList -> integral
decodeIntegral (CodedList []) = error "empty CodedList"
decodeIntegral (CodedList [wpped]) =
   let
      wrapped = fromIntegral wpped
   in
      if wrapped >= nextBit
         then
            wrapped - topBit
         else
            wrapped
decodeIntegral (CodedList (lPart : codedHigh)) =
   let
      lowestPart = fromIntegral lPart
      highPart = decodeIntegral (CodedList codedHigh)
   in
      lowestPart + (highPart `shiftL` bitsPerByte)

encodeWord :: (Integral integral,Bits integral) => integral -> CodedList
encodeWord (i :: integral) =
   if isLarge i
      then
         let
            lowestPart = i .&. mask
            highPart = i `shiftR` bitsPerByte
            CodedList codedHigh = encodeWord highPart
         in
            CodedList ((fromIntegral lowestPart) : codedHigh)
      else
         let
            wrapped = i
         in
            CodedList [fromIntegral wrapped]
   where
      isLarge :: integral -> Bool
      isLarge = (\ i -> i >= topBit)

decodeWord :: (Integral integral,Bits integral) => CodedList -> integral
decodeWord (CodedList []) = error "empty CodedList2"
decodeWord (CodedList [wpped]) =
   let
      wrapped = fromIntegral wpped
   in
      wrapped
decodeWord (CodedList (lPart : codedHigh)) =
   let
      lowestPart = fromIntegral lPart
      highPart = decodeWord (CodedList codedHigh)
   in
      lowestPart + (highPart `shiftL` bitsPerByte)

-- -----------------------------------------------------------------------
-- We make the word encoding (which is slightly more efficient for
-- unsigned integers) available via the Unsigned type.
-- -----------------------------------------------------------------------

-- | This is an @newtype@ alias for integral types where the user promises
-- that the value will be non-negative, and so saves us a bit.
-- This is what we use for character data incidentally, so that
-- ASCII characters with codes <128 can be encoded (as themselves) in
-- just one byte.
newtype Unsigned integral = Unsigned integral

instance (Monad m,Integral integral,Bits integral)
   => HasBinary (Unsigned integral) m where

   writeBin = mapWrite (\ (Unsigned i) -> encodeWord i)
   readBin = mapRead (\ i -> Unsigned (decodeWord i))

-- -----------------------------------------------------------------------
-- Bit constants
-- -----------------------------------------------------------------------

bitsInByte :: Int
-- Number of bits stored in a byte.  (
bitsInByte = 8

bitsPerByte :: Int
-- Number of bits of an integer we will store per char.
-- (The remaining one is used to mark the end of the sequence.)
bitsPerByte = bitsInByte - 1

-- Here are some useful abbreviations in this connection
topBit :: Bits integral => integral
topBit = bit bitsPerByte

mask :: (Integral integral,Bits integral) => integral
mask = topBit - 1

nextBit :: Bits integral => integral
nextBit = bit (bitsInByte - 2)

-- -----------------------------------------------------------------------
-- CodedList's.  These are used as an intermediate stage to integers.
-- -----------------------------------------------------------------------


newtype CodedList = CodedList [Byte]
-- This is a nonempty list of integers in [0,2^(bitsInByte-1)).
-- We code them by setting the top bit of all but the last item.

instance Monad m => HasBinary CodedList m where
   writeBin _ (CodedList []) = error "empty CodedList3"
   writeBin (WriteBinary {writeByte = writeByte}) (CodedList [b]) =
      writeByte b
   writeBin (wb @ WriteBinary {writeByte = writeByte}) (CodedList (b:bs)) =
      do
         writeByte (b .|. topBit)
         writeBin wb (CodedList bs)

   readBin (rb @ ReadBinary {readByte = readByte}) =
      do
         b <- readByte
         if b < topBit
            then
               return (CodedList [b])
            else
               do
                  (CodedList bs) <- readBin rb
                  return (CodedList ( (b `xor` topBit) :bs))


-- ----------------------------------------------------------------------
-- 5-way choices.  This is probably a bit clumsier than the HasWrapper
-- method (see next section), on the other hand perhaps a bit more
-- efficient for up to 5 alternatives, since decoding doesn't have to
-- hunt through the wraps list.
-- ----------------------------------------------------------------------

-- | This is a rather inelegant way of encoding a type with up to
-- 5 alternatives.  If 5 is too many, use () for the others, if too
-- few use 'HasWrapper'.  In fact 'HasWrapper' is probably better
-- anyway.
data Choice5 v1 v2 v3 v4 v5 =
      Choice1 v1
   |  Choice2 v2
   |  Choice3 v3
   |  Choice4 v4
   |  Choice5 v5 deriving (Eq)

instance (Monad m,
   HasBinary v1 m,HasBinary v2 m,HasBinary v3 m,HasBinary v4 m,HasBinary v5 m)
   => HasBinary (Choice5 v1 v2 v3 v4 v5) m
   where

   writeBin wb (Choice1 v) =
      do
         writeByte wb 1
         writeBin wb v
   writeBin wb (Choice2 v) =
      do
         writeByte wb 2
         writeBin wb v
   writeBin wb (Choice3 v) =
      do
         writeByte wb 3
         writeBin wb v
   writeBin wb (Choice4 v) =
      do
         writeByte wb 4
         writeBin wb v
   writeBin wb (Choice5 v) =
      do
         writeByte wb 5
         writeBin wb v

   readBin rb =
      do
         switch <- readByte rb
         case switch of
            1 ->
                do
                   v <- readBin rb
                   return (Choice1 v)
            2 ->
                do
                   v <- readBin rb
                   return (Choice2 v)
            3 ->
                do
                   v <- readBin rb
                   return (Choice3 v)
            4 ->
                do
                   v <- readBin rb
                   return (Choice4 v)
            5 ->
                do
                   v <- readBin rb
                   return (Choice5 v)
            _ -> fail ("BinaryInstances.Choice5 - unexpected switch "
               ++ show switch)

-- ----------------------------------------------------------------------
-- convenient (if inefficient) way of encoding algebraic datatypes.
-- ----------------------------------------------------------------------

-- | A class allowing you to handle types with up to 256 alternatives.
-- If this all seems to complicated, look at the source file and
-- the example for the \"Tree\" data type.
class HasWrapper wrapper m where
   wraps :: [Wrap wrapper m]
      -- ^ For each alternative in the type, provide a recognition
      -- 'Byte', and a way of mapping that alternative to the (wrapper)
   unWrap :: wrapper -> UnWrap m
      -- ^ Map a (wrapper) to the corresponding recognition 'Byte'
      -- and the type within the alternative.


-- | Newtype alias you need to wrap around something which instances
-- 'HasWrapper' to get an actual HasBinary instance.  You will then
-- need something like this:
--
-- > instance Monad m => HasBinary a m where
-- >   writeBin = mapWrite Wrapped
-- >   readBin = mapRead wrapped
--
newtype Wrapped a = Wrapped {wrapped :: a}

-- | Value the 'HasWrapper' instance generates from 'unWrap' to
-- indicate how we should write some value to binary.
data UnWrap m = forall val . HasBinary val m
   => UnWrap
      Byte --  label for this type on writing.
      val --  value inside this wrapped type.

-- | Some alternative the user provides in 'wraps' in the
-- 'HasWrapper' instance, to indicate one particular alternative we use
-- when reading from binary.
data Wrap wrapper m = forall val . HasBinary val m
   => Wrap
      Byte --  label for this type on reading.  This must, of course, be the
           -- same as for the corresponding UnWrap.
      (val -> wrapper)
           --  how to wrap this sort of value.

-- some abbreviations for construtor functions with varying numbers of
-- arguments.

-- | 'Wrap' value for constructor with no arguments.
wrap0 :: Monad m => Byte -> wrapper -> Wrap wrapper m
wrap0 label wrapper = Wrap label (\ () -> wrapper)


-- | 'Wrap' value for constructor with 1 argument.
wrap1 :: HasBinary val m => Byte -> (val -> wrapper) -> Wrap wrapper m
wrap1 = Wrap


-- | 'Wrap' value for constructor with 2 arguments.
wrap2 :: (HasBinary (val1,val2) m) => Byte
   -> (val1 -> val2 -> wrapper) -> Wrap wrapper m
wrap2 char con = Wrap char (\ (val1,val2) -> con val1 val2)


-- | 'Wrap' value for constructor with 3 arguments.
wrap3 :: (HasBinary (val1,val2,val3) m) => Byte
   -> (val1 -> val2 -> val3 -> wrapper) -> Wrap wrapper m
wrap3 char con = Wrap char (\ (val1,val2,val3) -> con val1 val2 val3)

-- | 'Wrap' value for constructor with 4 arguments.
wrap4 :: (HasBinary (val1,val2,val3,val4) m)
   => Byte -> (val1 -> val2 -> val3 -> val4 -> wrapper) -> Wrap wrapper m
wrap4 char con = Wrap char (\ (val1,val2,val3,val4) -> con val1 val2 val3 val4)

instance (Monad m,HasWrapper wrapper m) => HasBinary (Wrapped wrapper) m where
   writeBin wb (Wrapped wrapper) = writeBin' (unWrap wrapper)
      where
         writeBin' :: UnWrap m -> m ()
         writeBin' (UnWrap label val) =
            do
               writeBin wb label
               writeBin wb val

   readBin rb =
      do
         thisLabel <- readBin rb
         let
            innerWrap :: HasBinary v m => (v -> wrapper) -> m (Wrapped wrapper)
            innerWrap wrapFn =
               do
                  val <- readBin rb
                  return (Wrapped (wrapFn val))

         case findJust
            (\ (Wrap label wrapFn :: Wrap wrapper m) ->
               if label == thisLabel then Just (innerWrap wrapFn) else Nothing
               )
            (wraps :: [Wrap wrapper m]) of

            Nothing -> fail ("BinaryInstances.Wrapper - bad switch "
               ++ show thisLabel)
            Just (getWrap :: m (Wrapped wrapper)) -> getWrap

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f [] = Nothing
findJust f (x:xs) = case f x of
   (y@ (Just _)) -> y
   Nothing -> findJust f xs

{- Here is a little example -}
data Tree val =
      Leaf val
   |  Node [Tree val]

instance (Monad m,HasBinary val m) => HasWrapper (Tree val) m where
   wraps = [
      wrap1 0 Leaf,
      wrap1 1 Node
      ]
   unWrap = (\ wrapper -> case wrapper of
      Leaf v -> UnWrap 0 v
      Node l -> UnWrap 1 l
      )

instance (Monad m,HasWrapper (Tree val) m) => HasBinary (Tree val) m where
   writeBin = mapWrite Wrapped
   readBin = mapRead wrapped

-- ----------------------------------------------------------------------
-- HasBinary via Strings for things that are instances of Read/Show
-- ----------------------------------------------------------------------

-- | Newtype alias for things we want to encode or decode via their
-- 'Read' or 'Show' 'String' representation.
newtype ReadShow a = ReadShow a

instance (Read a,Show a,Monad m) => HasBinary (ReadShow a) m where
   writeBin = mapWrite (\ (ReadShow a) -> show a)
   readBin = mapRead (\ str ->
      case reads str of
         [(a,"")] -> ReadShow a
         _ -> error ("BinaryUtils.readBin -- couldn't parse " ++ show str)
      )

-- ----------------------------------------------------------------------
-- HasBinary via numbers for things that are instances of Enum.
-- ----------------------------------------------------------------------


newtype ViaEnum a = ViaEnum {enum :: a}

instance (Monad m,Enum a) => HasBinary (ViaEnum a) m where
   writeBin = mapWrite (\ (ViaEnum a)
      -> (fromEnum a) :: Int
      )
   readBin = mapRead (\ (aInt :: Int) -> ViaEnum (toEnum aInt))

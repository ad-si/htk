-- | This module provides a particular instance of the "Binary" modules
-- for binary encoding values.  What's new here is that the encoding
-- functions can also get hold of the 'View'.
--
-- We also require that instances of 'HasCodedValue' instance 'Typeable'.
-- This makes it easier to track down the type with the problem when
-- things go wrong.
module Types.CodedValue(
   HasCodedValue,
   CodedValue,
   CodingMonad, -- instance of MonadIO
   thisView, -- :: CodingMonad View

   doEncodeIO, -- :: HasCodedValue a => a -> View -> IO CodedValue
   doDecodeIO, -- :: HasCodedValue a => CodedValue -> View -> IO a
   equalByEncode,
      -- :: HasCodedValue value => (View,value) -> (View,value) -> IO Bool

   -- Functions for constructing instances (when mapWrite/mapRead aren't
   -- enough)
   mapWriteViewIO,
      -- :: HasBinary value2 CodingMonad
      -- => (View -> value1 -> IO value2) -- this you must provide
      -- -> WriteBinary CodingMonad -> value1 -> CodingMonad ()
   mapReadViewIO,
      -- :: HasBinary value2 CodingMonad
      -- => (View -> value2 -> IO value1) -- this you must provide
      -- -> ReadBinary CodingMonad -> CodingMonad value1
   mapReadPairViewIO,
      -- :: HasBinary typeKey CodingMonad
      -- => (View �> typeKey -> IO (WrappedRead value))
      -- -> ReadBinary CodingMonad -> CodingMonad value
      -- How to read values of wrapped type,
      --    eg WrappedLink, WrappedDisplayType, and friends.
      -- To write values, write a pair (typeKey,value2) where value2 is
      -- the value inside the wrapped type.
   WrappedRead(..),


   -- types and values exported for convenience from other modules.
   HasBinary(..),
   mapWrite,mapRead,mapWriteIO,mapReadIO,
   WrapBinary(..),


   ) where

import Control.Exception
import Control.Monad.State
import Control.Monad.Trans

import Util.BinaryAll
import Util.Dynamics
import Util.ICStringLen
import Util.ExtendedPrelude

import SimpleDB.ServerErrors

import Types.ViewType

-- --------------------------------------------------------------------------
-- The HasCodedValue class, now a synonym for several existing classes.
-- --------------------------------------------------------------------------

-- | This monad is used as the argument to the 'HasBinary' class.
newtype CodingMonad a = CodingMonad (ArgMonad View StateBinArea a)
   deriving (Monad)
   -- Thus instances of CodedValue will be able to write to a BinArea
   -- (so can be coded) and know the containing view.

unCodingMonad :: CodingMonad a -> ArgMonad View StateBinArea a
unCodingMonad (CodingMonad am) = am

-- | NB.  You don't instance this class, instead you declare
-- instances of 'Typeable' and 'HasBinary' and then 'HasCodedValue'
-- follows automatically.
class (Typeable ty,HasBinary ty CodingMonad) => HasCodedValue ty

instance (Typeable ty,HasBinary ty CodingMonad) => HasCodedValue ty


-- --------------------------------------------------------------------------
-- Monad trickery for CodingMonad
-- --------------------------------------------------------------------------

instance MonadIO CodingMonad where
   liftIO act = CodingMonad (liftIO act)

thisView :: CodingMonad View
thisView = CodingMonad (mkArgMonad return)

-- --------------------------------------------------------------------------
-- Values of type CodedValue
-- --------------------------------------------------------------------------

type CodedValue = ICStringLen

-- --------------------------------------------------------------------------
-- Reading and writing values of type CodedValue.
-- --------------------------------------------------------------------------


-- | Encode a value within a view.
doEncodeIO :: HasCodedValue a => a -> View -> IO CodedValue
doEncodeIO (a ::  a) view = doEncodeIO1 (show (typeOf a)) a view

doEncodeIO1 :: HasBinary a CodingMonad => String -> a -> View -> IO CodedValue
doEncodeIO1 desc (a ::  a) view =
   do
      encodeResult <- Control.Exception.try (
         do
            binArea1 <- mkEmptyBinArea 1024
            ((),binArea2) <-
               runStateT
                  (runArgMonad view
                     (unCodingMonad
                        (writeBin wb2 a)
                        )
                     )
                  binArea1
            bl <- closeBinArea binArea2
            bytesToICStringLen bl
         )

      case encodeResult of
         Left excep ->
            throwError ClientError
               ("Error " ++ show (excep :: SomeException)
                ++ " encoding " ++ desc)
         Right result -> return result
   where
      wb1 :: WriteBinary (ArgMonad View StateBinArea)
      wb1 = writeBinaryToArgMonad writeBinaryBinArea

      wb2 :: WriteBinary CodingMonad
      wb2 = liftWriteBinary CodingMonad wb1

-- | Decode a value in a view.
doDecodeIO :: HasCodedValue a => CodedValue -> View -> IO a
doDecodeIO codedValue view =
   let
      act = doDecodeIO1 desc codedValue view
      desc = show (typeOf (undefinedIO act))
   in
      act
   where
       undefinedIO :: IO a -> a
       undefinedIO _ = error "CodedValue.undefinedIO"


doDecodeIO1 :: forall a . HasBinary a CodingMonad => String -> CodedValue
            -> View -> IO a
doDecodeIO1 desc icsl view =
   do
      (decodeResult :: Either SomeException a) <- Control.Exception.try (
         do
            let
               bl = bytesFromICStringLen icsl
               binArea1 = mkBinArea bl

            (a,binArea2) <-
               runStateT
                  (runArgMonad view
                     (unCodingMonad
                        (readBin rb2)
                        )
                     )
                  binArea1

            checkFullBinArea binArea2
            touchICStringLen icsl
            return a
         )
      case decodeResult of
         Left excep ->
            do
               putStrLn ("Error " ++ show excep ++ " decoding "
                  ++ desc)
               throw excep
         Right result -> return result
   where
      rb1 :: ReadBinary (ArgMonad View StateBinArea)
      rb1 = readBinaryToArgMonad readBinaryBinArea

      rb2 :: ReadBinary CodingMonad
      rb2 = liftReadBinary CodingMonad rb1

-- | Check if two values are equal by comparing their encoding.
equalByEncode :: HasCodedValue value => (View,value) -> (View,value)
   -> IO Bool
equalByEncode vv1 vv2 =
   do
      ord <- compareByEncode vv1 vv2
      return (ord == EQ)

-- | Compare two values by comparing their encoding.
compareByEncode :: HasCodedValue value => (View,value) -> (View,value)
   -> IO Ordering
compareByEncode (view1,val1) (view2,val2) =
   do
      icsl1 <- doEncodeIO val1 view1
      icsl2 <- doEncodeIO val2 view2
      compareIO icsl1 icsl2


-- --------------------------------------------------------------------------
-- How to construct instances of HasCodedValue, when we've got
-- an IO action that depends on the view
-- --------------------------------------------------------------------------

-- | Used for constructing the 'writeBin' value inside 'HasBinary',
-- if you have a function which, given the view and the value, computes
-- something already an instance of 'HasBinary'
--
-- If you don't need the view, use 'mapWrite' or 'mapWriteIO' instead.
mapWriteViewIO :: HasBinary value2 CodingMonad
   => (View -> value1 -> IO value2)
   -> (WriteBinary CodingMonad -> value1 -> CodingMonad ())
mapWriteViewIO viewFn wb value1 =
   do
      view <- thisView
      value2 <- liftIO (viewFn view value1)
      writeBin wb value2

-- | Used for constructing the 'readBin' value inside 'HasBinary',
-- if you have a function which, given the view and something already
-- an instance of 'HasBinary', computes it.
--
-- If you don't need the view, use 'mapRead' or 'mapReadIO' instead.
mapReadViewIO :: HasBinary value2 CodingMonad
   => (View -> value2 -> IO value1)
   -> (ReadBinary CodingMonad -> CodingMonad value1)
mapReadViewIO viewFn rb =
   do
      value2 <- readBin rb
      view <- thisView
      liftIO (viewFn view value2)

-- --------------------------------------------------------------------------
-- Getting wrapped values where the type to be got is indexed by a typeKey.
-- The typeKey and wrapped value need to be written as an ordinary pair,
-- with the typeKey first.
-- --------------------------------------------------------------------------

data WrappedRead value =
   forall value2 .
      HasBinary value2 CodingMonad => WrappedRead value2 (value2 -> value)
      -- value2 is the type to be put inside a wrapped type.
      --
      -- The first argument to WrappedRead only gives the type, and is
      --    not evaluated.
      -- The second argument will be the constructor giving the value to
      --    to be returned.

mapReadPairViewIO :: HasBinary key CodingMonad
   => (View -> key -> IO (WrappedRead value))
   -> ReadBinary CodingMonad -> CodingMonad value
mapReadPairViewIO lookupFn rb =
   do
      key <- readBin rb
      view <- thisView
      wrappedRead <- liftIO (lookupFn view key)
      let
         doWrappedRead (WrappedRead _ fn) =
            do
               val <- readBin rb
               return (fn val)
      doWrappedRead wrappedRead



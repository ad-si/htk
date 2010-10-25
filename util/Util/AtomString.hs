{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- | AtomString atomises strings.  Right now this code
-- is not very efficient but it shouldn't be too hard
-- to improve.
--
-- This code includes no less that 3 uses of unsafePerformIO.  Oh well.
module Util.AtomString(
   AtomString,
      -- represents a string.  Instance of Ord, Eq, StringClass,
      -- Read and Show.  There is no guarantee that Ord on AtomString
      -- corresponds to Ord on the corresponding String.
   firstAtomString,
      -- :: AtomString
      -- However firstAtomString is guaranteed to be the first AtomString
      -- in the ordering.

   StringClass(..),
      -- encodes that a type encodes strings in some way.

   fromStringWEHacked,
   fromStringError,
      -- provide a primitive way for decoding String's to return an error.

   Str(..),
      -- WRAP


   mkFromStringWE,
      --  :: Parser stringClass -> String -> (String -> WithError stringClass)
      -- Make a fromStringWE function given a parser.
      -- The error message is of the form "/string/ is not a valid /typename/"
      -- where /typename/ is the first String argument to mkFromStringWE.
   ) where

import Control.Concurrent
import qualified Data.Map as Map
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Text.ParserCombinators.Parsec


import Util.QuickReadShow
import Util.Dynamics
import Util.DeepSeq
import Util.Computation
import Util.BinaryAll

data AtomSource = AtomSource (MVar (Map.Map BS.ByteString AtomString))
   -- where AtomStrings come from
   -- Here the key for an element is itself.


emptyAtomSource :: IO AtomSource
emptyAtomSource =
   do
      mVar <- newMVar Map.empty
      return (AtomSource mVar)

theAtomSource :: AtomSource
theAtomSource = unsafePerformIO emptyAtomSource
{-# NOINLINE theAtomSource #-}
-- avoid GHC bug with Linux optimisation which can clone MVars.

newtype AtomString = AtomString BS.ByteString deriving (Ord,Eq,Typeable)
-- in fact Eq could be unsafePtrEq

firstAtomString :: AtomString
firstAtomString = AtomString (BS.pack "")

------------------------------------------------------------------------
-- StringClass
------------------------------------------------------------------------

class StringClass stringClass where
   toString :: stringClass -> String

   -- We leave it up to the instance whether fromString or fromStringWE or both
   -- are defined.  Most of the time we only use fromString, but there are
   -- just a few cases (such as EntityNames) where we need fromStringWE.
   --
   -- For cases where we don't have fromStringWE fromStringWEHacked provides
   -- an alternative solution, if you can bear it.
   fromString :: String -> stringClass
   fromString s = coerceWithError (fromStringWE s)

   fromStringWE :: String -> WithError stringClass
   fromStringWE s = hasValue (fromString s)

instance StringClass AtomString where
   fromString string = unsafePerformIO (mkAtom string)

   toString atom = unsafePerformIO (readAtom atom)

instance StringClass stringClass => QuickRead stringClass where
   quickRead = WrapRead fromString

instance StringClass stringClass => QuickShow stringClass where
   quickShow = WrapShow toString

------------------------------------------------------------------------
-- We provide a way for instances of StringClass to return errors from
-- fromString by using the usual dreadful hack with Exception.
------------------------------------------------------------------------

fromStringWEHacked :: (StringClass stringClass,DeepSeq stringClass)
   => String -> IO (WithError stringClass)
fromStringWEHacked str =
   do
      either <- tryJust
         (\ dyn ->
               case fromDynamic dyn of
                  Nothing -> Nothing -- not a fromStringError.
                  Just (FromStringExcep mess) -> Just mess
            )
         (do
             let
                value = fromString str
             deepSeq value done
             return value
         )
      return (toWithError either)

fromStringError :: String -> a
fromStringError mess = throw $ toDyn (FromStringExcep mess)

newtype FromStringExcep = FromStringExcep String deriving (Typeable)

------------------------------------------------------------------------
-- StringClass instance
------------------------------------------------------------------------

mkAtom :: String -> IO AtomString
mkAtom str =
   do
      let
         packed = BS.pack str
         AtomSource mVar = theAtomSource

      map <- takeMVar mVar
      let
         (result,newMap) = case Map.lookup packed map of
            Nothing ->
               (AtomString packed,Map.insert packed (AtomString packed) map)
            Just newPacked -> (newPacked,map)
            -- now original copy of packed can be GC'd.
      putMVar mVar newMap
      return result


readAtom :: AtomString -> IO String
readAtom (AtomString packedString) =
   return(BS.unpack packedString)

------------------------------------------------------------------------
-- How to make a fromStringWE given a Parsec parser.
------------------------------------------------------------------------

mkFromStringWE :: Parser stringClass -> String
   -> (String -> WithError stringClass)
mkFromStringWE (parser0 :: Parser stringClass) typeName str =
   let
      parser1 =
         do
            result <- parser0
            eof
            return result
   in
      case parse parser1 "" str of
         Right stringClass -> hasValue stringClass
         Left _ -> hasError (show str ++ " is not a valid " ++ typeName)

------------------------------------------------------------------------
-- The Str class.  Wrapping an instance of StringClass in this gives
-- you an instance of HasBinary.
------------------------------------------------------------------------

newtype Str a = Str a

instance (Monad m,StringClass a) => HasBinary (Str a) m where
   writeBin = mapWrite (\ (Str a) -> toString a)
   readBin = mapRead (\ str -> Str (fromString str))

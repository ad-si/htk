{- AtomString atomises strings.  Right now this code
   is not very efficient but it shouldn't be too hard
   to improve.

   This code includes no less that 3 uses of unsafePerformIO.  Oh well.
   -}
module AtomString(
   AtomString, 
      -- represents a string.  Instance of Ord, Eq, StringClass, 
      -- Read and Show.  There is no guarantee that Ord on AtomString
      -- corresponds to Ord on the corresponding String.

   StringClass(..),
      -- encodes that a type encodes strings in some way.

   fromStringWEHacked,
   fromStringError,
      -- provide a primitive way for decoding String's to return an error.
   ) where               

import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO)
import PackedString
import Exception

import Debug(debug)
import QuickReadShow
import Dynamics
import DeepSeq
import Computation

data AtomSource = AtomSource (MVar (FiniteMap PackedString AtomString))
   -- where AtomStrings come from
   -- Here the key for an element is itself.


emptyAtomSource :: IO AtomSource
emptyAtomSource =
   do
      mVar <- newMVar emptyFM
      return (AtomSource mVar)

theAtomSource :: AtomSource
theAtomSource = IOExts.unsafePerformIO emptyAtomSource
{-# NOINLINE theAtomSource #-} 
-- avoid GHC bug with Linux optimisation which can clone MVars.

newtype AtomString = AtomString PackedString deriving (Ord,Eq)
-- in fact Eq could be unsafePtrEq

atomString_tyRep = mkTyRep "AtomString" "AtomString"
instance HasTyRep AtomString where
   tyRep _ = atomString_tyRep

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

instance StringClass AtomString where
   fromString string = IOExts.unsafePerformIO (mkAtom string)
   fromStringWE string = hasValue (fromString string)

   toString atom = IOExts.unsafePerformIO (readAtom atom)

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
         (\ exception -> case dynExceptions exception of
            Nothing -> Nothing -- don't handle this as it's not even a dyn.
            Just dyn ->
               case fromDyn dyn of
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

fromStringError mess = throwDyn (FromStringExcep mess)
            
newtype FromStringExcep = FromStringExcep String

fromStringExcep_tyRep = mkTyRep "ExtendedPrelude" "FromStringExcep"
instance HasTyRep FromStringExcep where
   tyRep _ = fromStringExcep_tyRep

------------------------------------------------------------------------
-- StringClass instance
------------------------------------------------------------------------

mkAtom :: String -> IO AtomString
mkAtom str =
   do
      let
         packed = packString str
         AtomSource mVar = theAtomSource

      map <- takeMVar mVar
      let
         (result,newMap) = case lookupFM map packed of
            Nothing -> 
               (AtomString packed,addToFM map packed (AtomString packed))
            Just newPacked -> (newPacked,map)
            -- now original copy of packed can be GC'd.
      putMVar mVar newMap
      return result

              
readAtom :: AtomString -> IO String
readAtom (AtomString packedString) =
   return(unpackPS packedString)


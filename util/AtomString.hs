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
   ) where               

import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO)
import PackedString

import Debug(debug)
import QuickReadShow
import Dynamics

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
   fromString :: String -> stringClass

instance StringClass AtomString where
   fromString string = IOExts.unsafePerformIO (mkAtom string)
   toString atom = IOExts.unsafePerformIO (readAtom atom)

instance StringClass stringClass => QuickRead stringClass where
   quickRead = WrapRead fromString

instance StringClass stringClass => QuickShow stringClass where
   quickShow = WrapShow toString

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


{- AtomString atomises strings.  Right now this code
   is not very efficient but it shouldn't be too hard
   to improve.
   -}
module AtomString(
   AtomString, -- represents a string.  Instance of Ord & Eq
   mkAtom, -- :: String -> IO AtomString
   readAtom -- :: AtomString -> IO String
   ) where               

import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO)
import PackedString

import Debug(debug)

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


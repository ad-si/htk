{- AtomString atomises strings.  Right now this code
   is not very efficient but it shouldn't be too hard
   to improve.
   -}
module AtomString(
   AtomSource, -- where AtomStrings come from
   emptyAtomSource, 
      -- :: IO AtomSource
      -- new source
   AtomString, -- represents a string.  Instance of Ord & Eq
   mkAtom, -- :: AtomSource -> String -> IO AtomString
   readAtom -- :: AtomSource -> AtomString -> IO String
   ) where               

import Concurrent
import FiniteMap
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

newtype AtomString = AtomString PackedString deriving (Ord,Eq)
-- in fact Eq could be unsafePtrEq

mkAtom :: AtomSource -> String -> IO AtomString
mkAtom (AtomSource mVar) str =
   do
      let
         packed = packString str
      map <- takeMVar mVar
      let
         (result,newMap) = case lookupFM map packed of
            Nothing -> 
               (AtomString packed,addToFM map packed (AtomString packed))
            Just newPacked -> (newPacked,map)
            -- now original copy of packed can be GC'd.
      putMVar mVar newMap
      return result

              
readAtom :: AtomSource -> AtomString -> IO String
readAtom _ (AtomString packedString) =
   return(unpackPS packedString)


{- This module generates short non-empty unique printable strings (IE without 
   funny characters).  Quotes and backslashes are not included, so printing
   should not be too hard.  Periods are also not included, for the
   benefit of NewNames.hs. -}
module UniqueString(
   UniqueStringSource, -- A source of unique strings.  Instance of Typeable
   newUniqueStringSource, -- :: IO UniqueStringSource
   newUniqueString, -- :: UniqueStringSource -> IO String


   -- Here is a "pure" interface.
   UniqueStringCounter,

   firstUniqueStringCounter, -- :: UniqueStringCounter
      -- This is what you start with
   stepUniqueStringCounter, -- :: UniqueStringCounter 
      -- -> (String,UniqueStringCounter)
      -- and this is how you get a new String out.

  
   -- read/createUniqueStringSource are used by types/CodedValue
   -- to import and export string sources.
   readUniqueStringSource, -- :: UniqueStringSource -> IO [Int]
   createUniqueStringSource, -- :: [Int] -> IO UniqueStringSource

   -- Create non-conflicting string which cannot be produced by
   -- newUniqueString.  This is useful for exceptional cases.
   newNonUnique, -- :: String -> String
   ) where

import Array
import Concurrent

import Dynamics
import QuickReadShow

-- The list of "printable" characters that may occur in one of these
-- strings.
printableCharsStr :: String
printableCharsStr =
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()"
   ++ "-_+=|~[{]};:,<>/?"

-- The same, as an array and length.
printableCharsLen :: Int
printableCharsLen = length printableCharsStr

printableCharsArr :: Array Int Char
printableCharsArr = listArray (0,printableCharsLen-1) printableCharsStr

-- -------------------------------------------------------------------
-- The impure interface.
-- -------------------------------------------------------------------

newtype UniqueStringSource = UniqueStringSource (MVar UniqueStringCounter)

newUniqueStringSource :: IO UniqueStringSource
newUniqueStringSource = 
   do
      mVar <- newMVar firstUniqueStringCounter
      return (UniqueStringSource mVar)
  
newUniqueString :: UniqueStringSource -> IO String
newUniqueString (UniqueStringSource mVar) =
   do
      uniqueStringCounter <- takeMVar mVar
      let 
         (str,nextUniqueStringCounter) = 
            stepUniqueStringCounter uniqueStringCounter
      putMVar mVar nextUniqueStringCounter
      return str

---
-- readUniqueStringSource is used by types/CodedValue.hs to export values.
readUniqueStringSource :: UniqueStringSource -> IO [Int]
readUniqueStringSource (UniqueStringSource mVar) =
   do
      (UniqueStringCounter l) <- readMVar mVar
      return l

---
-- createUniqueStringSource is the inverse of readUniqueStringSource.
createUniqueStringSource :: [Int] -> IO UniqueStringSource
createUniqueStringSource l = 
   do
      mVar <- newMVar (UniqueStringCounter l)
      return (UniqueStringSource mVar)

---
-- The instance is used by types/CodedValue
uniqueStringSource_tyCon = mkTyCon "UniqueString" "UniqueStringSource"
instance HasTyCon UniqueStringSource where
   tyCon _ = uniqueStringSource_tyCon

-- -------------------------------------------------------------------
-- The pure interface.
-- -------------------------------------------------------------------


-- UniqueStringCounter is a list of numbers from 0 to printableCharsLen-1.
-- The last number is at least 1.
newtype UniqueStringCounter = UniqueStringCounter [Int]

firstUniqueStringCounter :: UniqueStringCounter
firstUniqueStringCounter = UniqueStringCounter [0]

stepUniqueStringCounter :: UniqueStringCounter -> (String,UniqueStringCounter)
stepUniqueStringCounter (uniqueStringCounter @ (UniqueStringCounter ilist)) =
      (toStringUniqueStringCounter uniqueStringCounter,
         UniqueStringCounter (step ilist))
   where
      step [] = [1]
      step (first:rest) =
         if first == printableCharsLen -1
            then 
               0:step rest
            else
               (first+1):rest

toStringUniqueStringCounter :: UniqueStringCounter -> String
toStringUniqueStringCounter (UniqueStringCounter ilist) =
   map (\ i -> printableCharsArr ! i) ilist

-- -------------------------------------------------------------------
-- newNonUnique
-- -------------------------------------------------------------------

---
-- Create non-conflicting string which cannot be produced by
-- newUniqueString.  This is useful for exceptional cases.
-- We add this by adding a character with integer value 0 at the end.
newNonUnique :: String -> String
newNonUnique str = str ++ [printableCharsArr ! 0]

{- This module generates short non-empty unique printable strings (IE without 
   funny characters).  Quotes and backslashes are not included, so printing
   should not be too hard.  Periods are also not included, for the
   benefit of NewNames.hs. -}
module UniqueString(
   UniqueStringSource, -- A source of unique strings.
   newUniqueStringSource, -- :: IO UniqueStringSource
   newUniqueString, -- :: UniqueStringSource -> IO String


   -- Here is a "pure" interface.
   UniqueStringCounter,

   firstUniqueStringCounter, -- :: UniqueStringCounter
   stepUniqueStringCounter, -- :: UniqueStringCounter -> UniqueStringCounter
   toStringUniqueStringCounter, -- :: UniqueStringCounter -> String

   ) where

import Array
import Concurrent

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
      putMVar mVar (stepUniqueStringCounter uniqueStringCounter)
      return (toStringUniqueStringCounter uniqueStringCounter)

-- -------------------------------------------------------------------
-- The pure interface.
-- -------------------------------------------------------------------


-- UniqueStringCounter is a list of numbers from 0 to printableCharsLen-1.
newtype UniqueStringCounter = UniqueStringCounter [Int]

firstUniqueStringCounter :: UniqueStringCounter
firstUniqueStringCounter = UniqueStringCounter [0]

stepUniqueStringCounter :: UniqueStringCounter -> UniqueStringCounter
stepUniqueStringCounter (UniqueStringCounter ilist) =
      UniqueStringCounter (step ilist)
   where
      step [] = [0]
      step (first:rest) =
         if first == printableCharsLen -1
            then 
               0:step rest
            else
               (first+1):rest

toStringUniqueStringCounter :: UniqueStringCounter -> String
toStringUniqueStringCounter (UniqueStringCounter ilist) =
   map (\ i -> printableCharsArr ! i) ilist

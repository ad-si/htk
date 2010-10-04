-- | Test graph for RemoveAncestors.  This module is used with ghci.
-- Also for GraphOps.
module RemoveAncestorsTest where

import Graphs.RemoveAncestors
import Graphs.GraphOps

test :: [Int] -> IO ()
test l0 =
   do
      l1 <- removeAncestorsBy edges l0
      putStrLn (show l1)

test2 :: Int -> Int -> IO ()
test2 i1 i2 =
   do
      is <- isAncestorBy edges i1 i2
      putStrLn (show is)


edges :: Int -> IO [Int]
edges i = return (edges0 i)

edges0 :: Int -> [Int]
edges0 i = case i of
  1 -> [2,3]
  2 -> [4]
  3 -> [4,5]
  4 -> []
  5 -> [6]
  6 -> [7,8]
  7 -> [5]
  8 -> [8]


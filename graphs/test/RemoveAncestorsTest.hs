{- Test graph for RemoveAncestors.  This module is used with ghci. -}
module RemoveAncestorsTest where

import RemoveAncestors

test :: [Int] -> IO ()
test l0 =
   do
      l1 <- removeAncestorsBy edges l0
      putStrLn (show l1)
      

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
 
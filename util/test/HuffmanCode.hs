{- Test of Huffman folding using Huffman codes.  (There is no main function
   since this should be tested from the command line. -}
module HuffmanTree where

import Data.List(sort)

import Util.Huffman

data HuffmanTree a = Leaf Int a | Branch Int (HuffmanTree a) (HuffmanTree a)
  deriving Show
-- All integers should be non-negative.  The integer in a branch should be
-- the sum of the integers in the top Leaf or Branch of the two subbranches.

size :: HuffmanTree a -> Int
size (Leaf i _) = i
size (Branch i _ _) = i

branch :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
branch tree1 tree2 = Branch (size tree1 + size tree2) tree1 tree2

instance Eq (HuffmanTree a) where
   (==) tree1 tree2 = (==) (size tree1) (size tree2)

instance Ord (HuffmanTree a) where
   compare tree1 tree2 = compare (size tree1) (size tree2)

mkHuffmanTree :: [(Int,a)] -> HuffmanTree a
mkHuffmanTree values =
   let
      leaves = map (\ (i,a) -> Leaf i a) values
      sorted = sort leaves
   in
      huffmanFold branch sorted

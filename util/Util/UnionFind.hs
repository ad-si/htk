-- | Union-Find algorithm.
module Util.UnionFind(
   -- NB.  The functions in this module are not guaranteed thread-safe.


   UnionFind, -- :: type with parameter.  Instance of Eq.
   newElement, -- :: a -> IO (UnionFind a)
   toValue, -- :: UnionFind a -> a

   union, -- :: UnionFind a -> UnionFind a -> IO ()
   isSame, -- :: UnionFind a -> UnionFind a -> IO Bool
   sameElements, -- :: UnionFind a -> IO [UnionFind a]
   ) where

import Data.IORef

import Util.Computation(done)
import Util.ExtendedPrelude

-- -------------------------------------------------------------------
-- The datatype and instance of Eq
-- -------------------------------------------------------------------

data UnionFind a = UnionFind {
   value :: a,
   contentsRef :: IORef [UnionFind a],
      -- All items union'd with this one.
      -- Thus these contents form a tree-structure.
   headRef :: IORef (Maybe (UnionFind a))
      -- If Just, an item with which this one is union'd, possibly
      -- indirectly.
      --
      -- To avoid spending lots of time chasing up long chains of
      -- head pointers, we in each case replace the head with the eventual
      -- parent.  I think this is Tarjan's algorithm and makes the operations
      -- almost linear (amortized time), but can't be bothered to chase up
      -- the reference.
   }

instance Eq (UnionFind a) where
   (==) = mapEq contentsRef

-- -------------------------------------------------------------------
-- The external functions
-- -------------------------------------------------------------------

newElement :: a -> IO (UnionFind a)
newElement value =
   do
      contentsRef <- newIORef []
      headRef <- newIORef Nothing
      let
         unionFind = UnionFind {
            value = value,
            contentsRef = contentsRef,
            headRef = headRef
            }
      return unionFind

toValue :: UnionFind a -> a
toValue = value

union :: UnionFind a -> UnionFind a -> IO ()
union uf1 uf2 =
   do
      head1 <- getHead uf1
      head2 <- getHead uf2
      if head1 == head2
         then
            done
         else
            do
               writeIORef (headRef head2) (Just head1)

               contents0 <- readIORef (contentsRef head1)
               writeIORef (contentsRef head1) (head2 : contents0)

isSame :: UnionFind a -> UnionFind a -> IO Bool
isSame uf1 uf2 =
   do
      head1 <- getHead uf1
      head2 <- getHead uf2
      return (head1 == head2)

sameElements :: UnionFind a -> IO [UnionFind a]
sameElements uf =
   do
      head <- getHead uf
      allContents head
   where
      allContents :: UnionFind a -> IO [UnionFind a]
      allContents uf =
         do
            contents <- readIORef (contentsRef uf)
            innerContents <- mapM allContents contents
            return (uf : concat innerContents)

-- -------------------------------------------------------------------
-- Retrieving the head (the most important operation).
-- -------------------------------------------------------------------

getHead :: UnionFind a -> IO (UnionFind a)
getHead unionFind =
   do
      thisHeadOpt <- readIORef (headRef unionFind)
      case thisHeadOpt of
         Nothing -> return unionFind
         Just unionFind2 ->
            do
               thisHead <- getHead unionFind2
               writeIORef (headRef unionFind) (Just thisHead)
               return thisHead




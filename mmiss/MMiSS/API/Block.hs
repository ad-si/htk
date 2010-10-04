-- | MMiSSAPIBlock parses and constructs requests/responses containing a
-- XML element / sequence of datablocks (see the document
-- MMiSSProtocol.txt for a specification of the format).
module MMiSS.API.Block(
   Block,

   -- reading and writing blocks
   readBlock, -- :: Handle -> IO Block
   writeBlock, -- :: Handle -> Block -> IO ()

   -- constructing blocks
   BlockData(..),
   emptyBlock,
   addBlockData, -- :: Block -> BlockData -> (Block,Int)
      -- add one new BlockData and return its index.
   setBlockData, -- :: Block -> Int -> BlockData -> Block
      -- overwrite a pre-existing index in the Block.
   dummyBlockData, -- :: BlockData
      -- used as a dummy value when we want to leave a value specified later.

   -- deconstructing blocks
   lookupBlockData, -- :: Block -> Int -> Maybe BlockData
      -- Nothing if out of range.
   ) where

import IO

import qualified Data.Map as Map
import Data.Word

import Util.Bytes(Byte)
import Util.Binary
import Util.ICStringLen
import Util.AtomString

-- -------------------------------------------------------------------------
-- Datatypes
-- -------------------------------------------------------------------------

data BlockData = BlockData {
   blockType :: Byte,
   blockText :: ICStringLen
   }

data Block = Block (Map.Map Int BlockData)
   -- the keys will be consecutive integers starting at 0.
   -- This mean we can use Map.size to get the next index to use.


-- -------------------------------------------------------------------------
-- Reading and Writing
-- -------------------------------------------------------------------------

readBlock :: Handle -> IO Block
readBlock handle =
   do
      (nDataBlocks :: Word) <- hRead handle
      let
         readBlock :: IO BlockData
         readBlock =
            do
               blockType <- hRead handle
               blockText <- hRead handle
               return (BlockData {blockType = blockType,blockText = blockText})

      (mapList :: [(Int,BlockData)]) <-
         mapM
            (\ keyW ->
               do
                  blockData <- readBlock
                  return (fromIntegral keyW,blockData)
               )
            [ 0 .. nDataBlocks - 1 ]

      return (Block (Map.fromList mapList))

writeBlock :: Handle -> Block -> IO ()
writeBlock handle (Block fm) =
   do
      hWrite handle ((fromIntegral (Map.size fm)) :: Word)
      mapM_
         (\ blockData ->
            do
               hWrite handle (blockType blockData)
               hWrite handle (blockText blockData)
            )
         (Map.elems fm)

-- -------------------------------------------------------------------------
-- constructing blocks
-- -------------------------------------------------------------------------

emptyBlock :: Block
emptyBlock = Block Map.empty

addBlockData :: Block -> BlockData -> (Block,Int)
addBlockData (Block fm0) blockData =
   let
      key = Map.size fm0

      fm1 = Map.insert key blockData fm0
   in
      (Block fm1,key)

setBlockData :: Block -> Int -> BlockData -> Block
setBlockData (Block fm0) key blockData =
   case Map.lookup key fm0 of
      Nothing ->
         error ("MMiSSAPIBlock: Key " ++ show key ++ " does not exist")
      Just _ ->
         let
            fm1 = Map.insert key blockData fm0
         in
            Block fm1

dummyBlockData :: BlockData
dummyBlockData = BlockData {
   blockType = 255,
   blockText = fromString "ERROR"
   }

-- -------------------------------------------------------------------------
-- deconstructing blocks
-- -------------------------------------------------------------------------

lookupBlockData :: Block -> Int -> Maybe BlockData
lookupBlockData (Block fm) i = Map.lookup i fm

{-# LANGUAGE ScopedTypeVariables #-}

-- | Blockers are used to implement variable set sources which can be
-- turned on and off.  They are indexed by a BlockID.
module Util.VariableSetBlocker(
   Blocker,
   BlockID,
   newBlocker, -- :: HasKey a key => VariableSetSource a -> IO (Blocker a)
   newBlockID, -- :: IO BlockID

   openBlocker, -- :: HasKey a key => Blocker a -> BlockID -> IO ()
   closeBlocker, -- :: HasKey a key => Blocker a -> BlockID -> IO ()
   blockVariableSet,
      -- :: HasKey a key => Blocker a -> BlockID -> VariableSetSource a


   newBlockerWithPreAction
      -- :: HasKey a key => VariableSetSource a -> ([a] -> IO ())
      -- -> IO (Blocker a)
      --
      -- newBlockerWithPreAction creates a blocker that additionally permits
      -- an action that is performed the very first time the blocker is
      -- opened.
      -- The arguments to the action are the contents of the variable set
      -- at about the time of the opening.
   ) where

import System.IO.Unsafe
import Control.Concurrent

import Util.Object
import Util.Registry
import Util.Sink
import Util.Sources
import Util.VariableSet


-- --------------------------------------------------------------------
-- The types
-- --------------------------------------------------------------------

data Blocker a = Blocker {
   registry :: Registry BlockID (VariableSetSource a,Bool -> IO ()),
      -- For each blockID, the corresponding VariableSetSource and an
      -- action which blocks it, with True meaning "blocked".
   setSource :: VariableSetSource a
   }

newtype BlockID = BlockID ObjectID deriving (Eq,Ord)

-- --------------------------------------------------------------------
-- The functions
-- --------------------------------------------------------------------

newBlocker :: HasKey a key => VariableSetSource a -> IO (Blocker a)
newBlocker setSource =
   do
      registry <- newRegistry
      let
         blocker = Blocker {
            registry = registry,
            setSource = setSource
            }
      return blocker

newBlockerWithPreAction
   :: HasKey a key => VariableSetSource a -> ([a] -> IO ()) -> IO (Blocker a)
newBlockerWithPreAction setSource0 preAction =
   let
      action =
         do
            list <- readContents setSource0
            preAction list
      setSource1 = (unsafePerformIO action) `seq` setSource0
   in
      newBlocker setSource1

newBlockID :: IO BlockID
newBlockID =
   do
      objectID <- newObject
      return (BlockID objectID)

openBlocker :: HasKey a key => Blocker a -> BlockID -> IO ()
openBlocker blocker blockID =
   do
      (_,blockFn) <- getBlockEntry blocker blockID
      blockFn False

closeBlocker :: HasKey a key => Blocker a -> BlockID -> IO ()
closeBlocker blocker blockID =
   do
      (_,blockFn) <- getBlockEntry blocker blockID
      blockFn True

blockVariableSet :: HasKey a key
   => Blocker a -> BlockID -> IO (VariableSetSource a)
blockVariableSet blocker blockID =
   do
      (setSource,_) <- getBlockEntry blocker blockID
      return setSource

-- --------------------------------------------------------------------
-- The primitive functions
-- --------------------------------------------------------------------

getBlockEntry :: HasKey a key
   => Blocker a -> BlockID -> IO (VariableSetSource a,Bool -> IO ())
getBlockEntry blocker blockID =
   transformValue (registry blocker) blockID (\ entryOpt ->
      case entryOpt of
         Just entry -> return (entryOpt,entry)
         Nothing ->
            do
               entry <- blockableVariableSet (setSource blocker)
               return (Just entry,entry)
         )

-- | (setSource2,block) \<- blockableVariableSet setSource1
-- returns a setSource2 which is in one of two states.  In one state it is
-- blocked, and empty.  In the other, it is unblocked, and its contents are
-- the same as those of setSource1.  Initially it is blocked.  To switch
-- from one to the other the block function is used.  \"block True\" blocks
-- the set source; \"block False\" unblocks it.   Blocking if we are already
-- blocked, or unblocking if we are already unblocked, is harmless and does
-- nothing.
--
-- This somewhat baroque function is required for arc sets from folders.
-- I have wasted a couple of days trying to think of a more elegant way of
-- doing this ...
blockableVariableSet :: HasKey a key
   => VariableSetSource a -> IO (VariableSetSource a,Bool -> IO ())
blockableVariableSet (setSource1 :: VariableSetSource a) =
   do
      (mVar :: MVar (Maybe (IO ()))) <- newMVar Nothing
         -- If we are not blocked, contains the terminator action.
      set2 <- newEmptyVariableSet -- contains the contents of setSource2
      parallelX <- newParallelExec
         -- used to execute updates to set2.  This helps make sure they
         -- happen in the right order.
      let
         block doBlock = modifyMVar_ mVar (\ terminatorOpt ->
            do
               case (doBlock,terminatorOpt) of
                  (True,Just terminator) -> -- block
                     do
                        parallelExec parallelX (
                           do
                              terminator -- stop any more updates.
                              setVariableSet set2 [] -- empty this set.
                           )
                        return Nothing
                  (False,Nothing) -> -- unblock
                     do
                        sinkID <- newSinkID

                        let
                           doContents :: [a] -> IO ()
                           doContents contents = setVariableSet set2 contents

                           doUpdate :: VariableSetUpdate a -> IO ()
                           doUpdate update = updateSet set2 update

                        addNewSinkWithInitial setSource1 doContents doUpdate
                           sinkID parallelX
                        return (Just (invalidate sinkID))
                  _ -> return terminatorOpt
                  )

      return (toSource set2,block)

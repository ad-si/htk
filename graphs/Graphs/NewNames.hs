-- | NewNames is used for generating new names for Node's, Arc's,
-- NodeType's and ArcType's in a graph on a globally unique basis.
 module Graphs.NewNames (
   NameSource,
   NameSourceBranch, -- instance of Show/Read
   branch, -- :: NameSource -> IO NameSourceBranch
   useBranch, -- :: NameSourceBranch -> IO NameSource
   -- To make a new separate root use branch followed by useBranch
   initialBranch, -- :: NameSourceBranch
   -- Use this with useBranch to start the thing off.

   getNewName, -- :: NameSource -> IO String
   -- These strings always begin with a '.'.

   FrozenNameSource,  -- instance of Read/Show

   freezeNameSource, -- :: NameSource -> IO FrozenNameSource
   defrostNameSource, -- :: NameSource -> FrozenNameSource -> IO ()
   -- freeze/defrostNameSource convert and restore the current name source
   -- to and from a string.
   -- defrostNameSource should be handed a NameSource created from the
   -- same NameSourceBranch as that for which freezeNameSource was
   -- called, otherwise it raises an error.
   ) where

import Util.Computation

import Control.Concurrent

data NameSource = NameSource {
   nameSourceId :: [Int],
   branchCounter :: MVar Int,
   nameCounter :: MVar Int
   -- Locking policy.  Either branchCounter or nameCounter can be emptied
   -- separately, but may only remain empty for a short time in which
   -- no other locking/unlocking operations are done.
   -- If the two are emptied together, branchCounter should be emptied
   -- first.
   }

-----------------------------------------------------------------------------
-- Creating and branching NameSource's
-----------------------------------------------------------------------------

newtype NameSourceBranch = NameSourceBranch [Int] deriving (Read,Show)

branch :: NameSource -> IO NameSourceBranch
branch (NameSource
      {nameSourceId = nameSourceId,branchCounter = branchCounter}) =
   do
      branchNo <- takeMVar branchCounter
      putMVar branchCounter (branchNo+1)
      return (NameSourceBranch (branchNo:nameSourceId))

useBranch :: NameSourceBranch -> IO NameSource
useBranch (NameSourceBranch nameSourceId) =
   do
      branchCounter <- newMVar 0
      nameCounter <- newMVar 0
      return (NameSource {
         nameSourceId = nameSourceId,
         branchCounter = branchCounter,
         nameCounter = nameCounter
         })


initialBranch :: NameSourceBranch
initialBranch = NameSourceBranch []

-----------------------------------------------------------------------------
-- Getting new strings
-----------------------------------------------------------------------------

getNewName :: NameSource -> IO String
getNewName
      (NameSource {nameSourceId = nameSourceId,nameCounter=nameCounter}) =
   do
      nameNo <- takeMVar nameCounter
      putMVar nameCounter (nameNo+1)
      return (listToString (nameNo:nameSourceId))

listToString :: [Int] -> String
-- produces compact representation of the argument beginning with a period.
listToString numbers = concat (map (\ n -> '.':(show n)) numbers)

-----------------------------------------------------------------------------
-- freeze/restoreNameSource
-----------------------------------------------------------------------------

data FrozenNameSource = FrozenNameSource {
   frozenId :: [Int],
   frozenBranch :: Int,
   frozenName :: Int
   } deriving (Read,Show)

freezeNameSource :: NameSource -> IO FrozenNameSource
freezeNameSource (NameSource {
      nameSourceId = nameSourceId,
      branchCounter = branchCounter,
      nameCounter = nameCounter
      }) =
   do
      frozenBranch <- takeMVar branchCounter
      frozenName <- takeMVar nameCounter
      putMVar nameCounter frozenName
      putMVar branchCounter frozenBranch
      return (FrozenNameSource {
         frozenId = nameSourceId,
         frozenBranch = frozenBranch,
         frozenName = frozenName
         })

defrostNameSource :: NameSource -> FrozenNameSource -> IO ()
defrostNameSource
   (NameSource {
      nameSourceId = nameSourceId,
      branchCounter = branchCounter,
      nameCounter = nameCounter
      })
   (FrozenNameSource {
      frozenId = frozenId,
      frozenBranch = frozenBranch,
      frozenName = frozenName
      }) =
   do
      let
         fail mess =
            ioError(userError("NewNames.defrostNameSource: "++mess))

      if (nameSourceId /= frozenId)
         then
            fail "Name source mismatch"
         else
            done

      oldBranch <- takeMVar branchCounter
      putMVar branchCounter frozenBranch

      oldName <- takeMVar nameCounter
      putMVar nameCounter frozenName




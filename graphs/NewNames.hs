{- NewNames is used for generating new names for Node's, Arc's, 
   NodeType's and ArcType's in a graph on a globally unique basis.
   -}
 module NewNames (
   NameSource,
   NameSourceBranch, -- instance of Show/Read
   branch, -- :: NameSource -> IO NameSourceBranch
   useBranch, -- :: NameSourceBranch -> IO NameSource
   -- To make a new separate root use branch followed by useBranch
   initialBranch, -- :: NameSourceBranch
   -- Use this with useBranch to start the thing off.
                 
   getNewName, -- :: NameSource -> IO String
   -- These strings always begin with a '.'.
   ) where

import Concurrent

data NameSource = NameSource {
   nameSourceId :: [Int],
   branchCounter :: MVar Int,
   nameCounter :: MVar Int
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

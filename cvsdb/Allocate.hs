{- Allocate is used by the allocator to generate new
   versions and CVS file names. 

   It is here because firstCVSFile, secondCVSFile and firstCVSVersion
   are also used by CVSDB.
   -}
module Allocate(
   AllocateState, -- things known about.  Instance of Show and Read.
   initialAllocateState, -- :: For when nothing is allocated.

   newCVSFile, -- :: AllocateState -> (AllocateState,CVSFile)
   -- allocates a new CVSFile.
   newCVSVersion, -- :: AllocateState -> CVSFile -> CVSVersion ->
   --   (AllocateState,CVSVersion)
   -- create a new pseudo-version (suitable for passing to
   -- the cvsCommit function) which is a successor to the
   -- one supplied.

   firstCVSFile, -- :: CVSFile
   secondCVSFile, -- :: CVSFile
   -- These are the first two CVSFiles allocated.

   firstCVSVersion, -- :: CVSVersion
   -- This should be the very first version a CVS file receives, IE 1.1
   ) where
import Char

import FiniteMap

import QuickReadShow
import FileNames
import UniqueFile

import CVSTypes

data AllocateState = AllocateState {
-- AllocateState consists of two completely independent parts,
-- one for generating CVSFiles and one for generating CVSVersions.
   encodedName :: UniqueFileCounter, -- next new CVSFile in an encoded form.
   versionMap :: VersionMap -- state for generating new versions.
   } deriving (Read,Show)


-- Allocate the first two names
allocateState0 :: AllocateState
allocateState0 = AllocateState {
   encodedName = initialUniqueFileCounter,
   versionMap = initialVersionMap
   }

allocateResult1 :: (AllocateState,CVSFile)
allocateResult1 = newCVSFile allocateState0

firstCVSFile :: CVSFile
firstCVSFile = snd allocateResult1

allocateState1 :: AllocateState
allocateState1 = fst allocateResult1

allocateResult2 :: (AllocateState,CVSFile)
allocateResult2 = newCVSFile allocateState1

secondCVSFile :: CVSFile
secondCVSFile = snd allocateResult2

initialAllocateState :: AllocateState
initialAllocateState = fst allocateResult2


firstCVSVersion :: CVSVersion
firstCVSVersion = CVSVersion "1.1"

---------------------------------------------------------------------
-- Allocating a new CVSFile.
---------------------------------------------------------------------

newCVSFile :: AllocateState -> (AllocateState,CVSFile)
newCVSFile (allocateState@AllocateState{encodedName = encodedName0}) =
   let
      (str,encodedName1) = stepUniqueFileCounter encodedName0
   in
      (allocateState {encodedName = encodedName1},CVSFile str)

---------------------------------------------------------------------
-- Allocating a new CVSVersion.
---------------------------------------------------------------------

newCVSVersion :: AllocateState -> CVSFile -> CVSVersion -> 
   (AllocateState,CVSVersion)
newCVSVersion allocateState cvsFile cvsVersion =
   let
      map = versionMap allocateState
      (newMap,newCVSVersion) = newCVSVersionPrim map cvsFile cvsVersion
   in
      (allocateState{versionMap=newMap},newCVSVersion)

initialVersionMap = VersionMap emptyFM

newtype VersionMap = VersionMap(FiniteMap (CVSFile,CVSVersion) Int)
-- For all versions for which we have already supplied a successor
-- this provides a number for the next branch to use.  If this isn't
-- clear look at the next function . . .

type ShowableMap = [((CVSFile,CVSVersion),Int)]

instance QuickRead VersionMap where
   quickRead = 
      WrapRead (\ (list :: ShowableMap) -> VersionMap (listToFM list))

instance QuickShow VersionMap where
   quickShow =
      WrapShow (\ (VersionMap map) -> (fmToList map) :: ShowableMap)

newCVSVersionPrim :: VersionMap -> CVSFile -> CVSVersion ->
   (VersionMap,CVSVersion)
newCVSVersionPrim (VersionMap versionMap) cvsFile cvsVersion =
   case lookupFM versionMap cvsFileVersion of
      Nothing ->
      -- We have not previously allocated a successor to this version.
      -- Therefore we simply bump the last component of the version;
      -- EG "1.4" goes to "1.5".
         (VersionMap(addToFM versionMap cvsFileVersion 1),
            bumpVersion cvsVersion)
      Just nextBranch ->
      -- We have allocated a successor to this version, so we must
      -- add a branch with number nextBranch.
         (VersionMap(addToFM versionMap cvsFileVersion (nextBranch+1)),
            addBranch cvsVersion nextBranch
            )
   where
      cvsFileVersion = (cvsFile,cvsVersion)

      addBranch :: CVSVersion -> Int -> CVSVersion
      addBranch (CVSVersion str) branchNo =
         CVSVersion(str ++ ('.':(show branchNo)))

      bumpVersion :: CVSVersion -> CVSVersion
      bumpVersion (CVSVersion str) =
         let
            (beforeDot,afterDot) = 
               case splitLastDot str of
                  Just result -> result
                  Nothing -> error "CVSFileServer giving a bad version number"
            -- this will raise an error if there is no dot in the string.
            branch = read afterDot :: Int
         in
            addBranch (CVSVersion beforeDot) (branch+1)

      splitLastDot :: String -> Maybe (String,String)
      splitLastDot [] = Nothing
      splitLastDot ('.':rest) =
         case splitLastDot rest of
            Nothing -> Just ("",rest)
            Just (beforeDot,afterDot) -> Just ('.':beforeDot,afterDot)
      splitLastDot (c:rest) =
         case splitLastDot rest of
            Nothing -> Nothing
            Just (beforeDot,afterDot) -> Just(c:beforeDot,afterDot)
 
            
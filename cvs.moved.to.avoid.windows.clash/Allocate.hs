{- Allocate is used by the allocator to generate new
   versions and CVS file names. 

   It is here because initialCVSFile is also used by CVSDB.

   For testing purposes this module should be compilable by Hugs.
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

   initialCVSFile -- :: CVSFile
   -- Should be #2(newINode(initialAllocateState))
   ) where
import Char

import FiniteMap

import FileNames

import CVSTypes

data AllocateState = AllocateState {
-- AllocateState consists of two completely independent parts,
-- one for generating CVSFiles and one for generating CVSVersions.
   encodedName :: EncodedName, -- next new CVSFile in an encoded form.
   versionMap :: VersionMap -- state for generating new versions.
   }

initialAllocateState :: AllocateState
initialAllocateState = AllocateState {
   encodedName = initialEncodedName,
   versionMap = initialVersionMap
   }

initialCVSFile :: CVSFile
initialCVSFile =
   let
      (_,cvsFile) = newCVSFile initialAllocateState
   in
      cvsFile

---------------------------------------------------------------------
-- Allocating a new CVSFile.
---------------------------------------------------------------------

{- 
   Strategy: each file name has the form 
   [char]/[char]/.../[char]
   followed (on the repository) by ",v".
   The [char] is chosen from the 64-character set:

   lower case and upper case letters (52)
   digits (10)
   @+
   
   Thus each char corresponds to a number between 0 and 63.
   The characters are divided into those with numbers <22
   and those with numbers >=22.  Characters with numbers >=22 
   correspond to bits of the directory entry of the file name.
   The ones with numbers <22 correspond to the file name part.
   Thus the file names can get arbitrarily long.  The reason
   for choosing 22 is that it maximises the number of possibilities
   when there are up to three parts, which is 39754.
   -}

newCVSFile :: AllocateState -> (AllocateState,CVSFile)
newCVSFile allocateState =
   let
      encoded = encodedName allocateState
      nextAllocateState = allocateState {encodedName=increment encoded}
      name = CVSFile(toString encoded)
   in
      (nextAllocateState,name)


type EncodedName = [Int]
-- name to be generated encoded as ints, last component (of filename) 
-- first in list.

initialEncodedName :: EncodedName
initialEncodedName = [0]

toString :: EncodedName -> String
toString (first:rest) = tS [encodeChar first] rest
   where
      tS :: String -> [Int] -> String
      tS acc [] = acc
      tS acc (first:rest) = tS ((encodeChar first):fileSep:acc) rest

encodeChar :: Int -> Char
encodeChar i=
   if i<26 then
      chr(ord 'a' + i)
   else if i<52 then
      chr((ord 'A'-26)+i)
   else if i<62 then
      chr((ord '0'-52)+i)
   else case i of
      62 -> '@'
      63 -> '+'

increment :: EncodedName -> EncodedName
increment (file:rest) =
   if file==(divider-1) 
      then
         0:(incrementDirs rest)
      else
         (file+1):rest
   where
      incrementDirs :: [Int] -> [Int]
      incrementDirs [] = [divider]
      incrementDirs (first:rest) =
         if first==(nChars-1) 
            then
               divider:(incrementDirs rest)
            else
               (first+1):rest    


divider :: Int
divider = 22

nChars :: Int
nChars = 64

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
 
            
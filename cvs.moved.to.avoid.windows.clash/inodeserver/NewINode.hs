{- A NewINode.INodeSource runs on a machine hosting a CVS repository.
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

   There should only be one INodeSource per repository+prefix, or the
   results are undefined.

   Attribute Files.  CVSDB.hs also includes code for attributes,
   which are stored in "attribute files" with names identical to
   that of the file, with a # appended.
   -}
module NewINode(
   INodeSource,
   mkINodeSource, -- :: String -> IO INodeSource
   -- makes an INodeSource given the name of
   -- the directory to start from.  
   newINode, -- :: INodeSource -> IO String
   ) where

import Char
import Directory
import Concurrent

import FileNames

data INodeSource = INodeSource{
   nextName :: MVar EncodedName
   }

mkINodeSource :: String -> IO INodeSource
mkINodeSource prefix =
   -- we simply scan through all inodes.  A faster way would be to
   -- use divide and conquer, but I can't be bothered.
   do      
      next <- searchINode initial
      nextName <- newMVar next
      return(INodeSource{nextName=nextName})
   where
      pref = trimDir prefix

      searchINode :: EncodedName -> IO EncodedName 
      searchINode nextToTry =
         do
            let
               name = pref ++ [fileSep] ++ (toString nextToTry) ++ ",v"
            exists <- doesFileExist name
            if exists 
               then
                  searchINode(increment nextToTry)
               else
                  return nextToTry

newINode :: INodeSource -> IO String
newINode (INodeSource{nextName=nextName}) =
   do
      next <- takeMVar nextName
      putMVar nextName (increment next)
      return (toString next)

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

type EncodedName = [Int]
-- name to be generated encoded as ints, last component (of filename) 
-- first in list.

initial :: EncodedName
initial = [0]

toString :: EncodedName -> String
toString (first:rest) = tS [encodeChar first] rest
   where
      tS :: String -> [Int] -> String
      tS acc [] = acc
      tS acc (first:rest) = tS ((encodeChar first):fileSep:acc) rest

divider :: Int
divider = 22

nChars :: Int
nChars = 64

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




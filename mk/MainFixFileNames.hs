{- This program converts stdin to stdout.  It replaces all occurrences of
   the string "$PWD" in stdin with the current directory in which it is
   executed.  Everything else is left unchanged.

   The program occupies an unusual place in the UniForM sources; it is not
   part of the main sources, but is only used during building (see suffix.mk)
   and is compiled during configuration.  Since it only uses completely
   standard Haskell 98, this ought to be pretty easy.

   We do things this way rather than with some sort of script so that this
   will work even in the extremely hostile environment of Windows (with no
   cygwin).  Also, using GHC's getCurrentDirectory means we get at what
   GHC thinks the current directory is (IE the Windows file name) rather than
   what it is in cygwin's Unix world.
   -}
module Main (main) where

import Directory

main :: IO ()
main = 
   do
      input <- getContents
      currentDirectory  <- getCurrentDirectory
      let
         transform [] = []
         transform ('$':'P':'W':'D':rest) = currentDirectory ++ transform rest
         transform (c:rest) = c:transform rest
      putStr . transform  $ input


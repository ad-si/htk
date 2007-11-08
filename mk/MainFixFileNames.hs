{- This program converts stdin to stdout.  It converts all occurrences of
   the string "#"++key in stdin with the current directory in which it is
   executed, C escaped.  The key is by default "PWD", unless an argument
   is supplied, in which case that is used.

   The program occupies an unusual place in the UniForM sources; it is not
   part of the main sources, but is only used during building (see suffix.mk)
   and is compiled during configuration.  Since it only uses completely
   standard Haskell 98, this ought to be pretty easy.

   We do things this way rather than with some sort of script so that this
   will work even in the extremely hostile environment of Windows (with no
   cygwin).  Also, using GHC's getCurrentDirectory means we get at what
   GHC thinks the current directory is (IE the Windows file name) rather than
   what it is in cygwin or MinGW's Unix world.
   -}
module Main (main) where

import Directory
import System

main :: IO ()
main =
   do
      input <- getContents
      args <- getArgs
      toInsert <- getCurrentDirectory
      let
         key = case args of
           [arg] -> arg
           [] -> "PWD"

         escapeString s =
            let
               withQuotes @ ('\"':rest) = show s
            in
               take (length rest - 1) rest

         quoted = escapeString toInsert


         transform [] = []
         transform (s@(c:rest)) = case isPrefix ("#"++key) s of
            Nothing -> c:transform rest
            Just rest -> quoted ++ transform rest
      putStr . transform  $ input

isPrefix :: Eq a => [a] -> [a] -> Maybe [a]
isPrefix [] s = Just s
isPrefix (c1 : c1s) (c2 : c2s) | c1 == c2
   = isPrefix c1s c2s
isPrefix _ _ = Nothing




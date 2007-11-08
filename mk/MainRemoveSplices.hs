{- This module is used to filter modules before giving them to Haddock,
   to remove all splices, and replace them, if expressions, by an error
   call.

   The strategy for determining whether something is a declaration or
   expression splice is simple: every splice that comes immediately after
   "\n" is a declaration splice.  Not nice I know, but probably true.
   -}
module Main where

main =
   do
      s <- getContents
      let
         t = removeSplices s
      putStr t

removeSplices :: String -> String
removeSplices ('\n':'$':'(':rest) = removeSplices (skipPars 0 rest)
removeSplices ('$':'(':rest) =
   " (error \"Splice Removed for Haddock\") "
      ++ removeSplices (skipPars 0 rest)
removeSplices (c:rest) = c:removeSplices rest
removeSplices [] = []

skipPars :: Int -> String -> String
skipPars 0 (')':rest) = rest
skipPars n ('(':rest) = skipPars (n+1) rest
skipPars n (')':rest) = skipPars (n-1) rest
skipPars n (c:rest) = skipPars n rest

{- TestRegularExpression is used for experimenting with regular
   expressions.  -}
module Main(main) where

import RegularExpression

-- testPair regex string 
-- prints result of matching string against regex.
testPair :: String -> String -> IO ()
testPair regex string =
   let
      compiled =  compile regex
      result = matchString compiled string
   in
      putStr (
         case result of
            Nothing -> "No Match\n"
            Just result -> (show result) ++ "\n"
         )

main :: IO ()
main =
   do
      regex <- getLine
      string <- getLine
      testPair regex string
      main

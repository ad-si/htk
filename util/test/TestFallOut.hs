{- Test ExtendedPrelude.addFallOut -}
module Main(main) where

import Util.ExtendedPrelude

main =
   do
      putStrLn "Type 1 for break 1, 2 for break 2, E for error\n"
      result <- addFallOut (\ break1 ->
         addFallOut (\ break2 ->
            do
               c <- getChar
               case c of
                  '1' -> break1 "break 1"
                  '2' -> break2 "break 2"
                  'E' -> error "Error exit"
                  c -> return (c:":Normal exit")
            )
         )
      putStrLn (show result)

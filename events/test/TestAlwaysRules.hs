{- This module checks that the special rules for doing sync(always) work.
   (They will only do so if we compile with optimisation on).
   -}
module Main(main) where

import Object
import Events

beforeAfter :: IO () -> IO ()
beforeAfter action =
   do
      before <- newInt
      putStrLn (show before)
      action
      after <- newInt
      putStrLn (show after)

main :: IO ()
#if 0

main = (beforeAfter . sync) (always (putStrLn "T1") >>>= (\ _ -> sync (always (putStrLn "T2"))))

#else

main = (beforeAfter . sync) (
   do
      always (putStrLn "T1")      
      always (putStrLn "T2")
      always (putStrLn "T3")
      always (putStrLn "T4")
      always (putStrLn "T5")
   )
#endif
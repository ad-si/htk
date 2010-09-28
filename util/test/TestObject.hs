{- Test Object.hs.  We need this because it seems to be misbehaving
   on Linux -}
module Main(main) where


import Util.Object

main :: IO ()
main =
   do
      o1 <- newObject
      o2 <- newObject
      o3 <- newObject
      o4 <- newObject
      o5 <- newObject
      o6 <- newObject
      o7 <- newObject
      putStr(show[o1,o2,o3,o4,o5,o6,o7])

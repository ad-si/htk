module Main (main) where

import Dynamics
import qualified Dynamic
import Debug(debug)

data A = A (Int,Int) deriving Show
data B = B (Bool,Bool) deriving Show

aTc = Dynamic.mkTyCon "A"

instance Typeable A where
   typeOf _ = Dynamic.mkAppTy aTc []

instance Typeable B where
   typeOf _ = mkTypeTag "DynamicsTest" "B" []
   
main :: IO()
main =
   let
      dynOne = toDyn (1 :: Int)
      dynH = toDyn "Hello"
      dynA = toDyn (A(1,1))
      dynB = toDyn (B(True,False))
   in
      do 
         (one :: Int) <- coerceIO dynOne
         print one
         (h :: String) <- coerceIO dynH
         print h
         (a :: A) <- coerceIO dynA
         print a
--         (b :: B) <- coerceIO dynB
--         print b



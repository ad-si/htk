-- | Integers augmented with Infinity.
module Util.IntPlus(
   IntPlus,
   infinity
   ) where

-- --------------------------------------------------------------------
-- The datatype
-- --------------------------------------------------------------------

-- | The Bool is a sign, with True meaning positive infinity.
data IntPlus = Infinite Bool | Finite Integer deriving Eq

-- --------------------------------------------------------------------
-- The interface
-- --------------------------------------------------------------------

infinity :: IntPlus
infinity = Infinite True

instance Ord IntPlus where
   compare i1 i2 = case (i1,i2) of
      (Infinite b1,Infinite b2) -> compare b1 b2
      (Finite _,Infinite b) -> if b then LT else GT
      (Infinite b,Finite _) -> if b then GT else LT
      (Finite i1,Finite i2) -> compare i1 i2

instance Show IntPlus where
   showsPrec _ (Infinite b) s = if b then "infinity"++s else "-infinity"++s
   showsPrec p (Finite i) s = showsPrec p i s

instance Num IntPlus where
   (+) i1 i2 = case (i1,i2) of
      (Finite i1,Finite i2) -> Finite (i1 + i2)
      (Infinite b,Finite _) -> Infinite b
      (Finite _,Infinite b) -> Infinite b
      (Infinite b1,Infinite b2) ->
         if b1 == b2 then Infinite b1 else
            error "IntPlus: attempt to subtract infinities of like sign"
   (*) i1 i2 = case (i1,i2) of
      (Finite i1,Finite i2) -> Finite (i1*i2)
      (Finite i,Infinite b) -> mul i b
      (Infinite b,Finite i) -> mul i b
      (Infinite b1,Infinite b2) -> Infinite (b1 == b2)
      where
         mul i b = case compare i 0 of
            LT -> Infinite (not b)
            EQ -> Finite 0
            GT -> Infinite b

   negate (Finite i) = Finite (negate i)
   negate (Infinite b) = Infinite (not b)

   abs (Finite i) = Finite (abs i)
   abs (Infinite _) = infinity

   signum i = case compare i 0 of
      LT -> -1
      EQ -> 0
      GT -> 1

   fromInteger i = Finite i




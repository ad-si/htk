{- Factorial written efficiently using huffmanFold -}
module Main(main,fact,fact1,fact2,fact3) where


import Util.Huffman

fact :: Integer -> Integer
fact n =
   if n<2
   then
      if n<0 then error "factorial of negative numbers is infinite" else 1
   else
      huffmanFold (*) [2..n]

main :: IO ()
main =
   do
      s <- getLine
      let
         n = read s
         fn = fact n
      if fn>0 then putStrLn "Factorial computed" else error "Impossible!"
      print fn


-- Some competing implementations of fact
-- (1) the first and second "fastest" from
--     http://www.willamette.edu/~fruehr/haskell/evolution.html
fact1 :: Integer -> Integer
s f g x = f x (g x)
k x y   = x
b f g x = f (g x)
c f g x = f x g
y f     = f (y f)
cond p f g x = if p x then f x else g x
fact1  = y (b (cond ((==) 0) (k 1)) (b (s (*)) (c b pred)))

fact2 :: Integer -> Integer
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fact2 = facAcc 1

-- (2) My original effort to do better
fact3 :: Integer -> Integer
fact3 n =
  if n<2
  then
     if n>=0 then 1 else error "Negative factorials are infinite"
  else
     pairProd [2..n]

pairProd :: [Integer] -> Integer
pairProd [x] = x
pairProd l = pairProd(multiplyPairs l [])

multiplyPairs :: [Integer] -> [Integer] -> [Integer]
multiplyPairs [] d = d
multiplyPairs [x] d = x:d
multiplyPairs (x1:x2:xs) d = multiplyPairs xs ((x1*x2):d)




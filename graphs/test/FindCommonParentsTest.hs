-- | Test graphs for FindCommonParents 
module FindCommonParentsTest where

import FindCommonParents

graph1 :: GraphBack Int Int
graph1 = GraphBack {
   getAllNodes = return [1..9],
   getKey = (\ i -> return (Just i)),
   getParents = (\ i -> return (Just (case i of
      1 -> []
      2 -> [1]
      3 -> [1,9]
      4 -> [2]
      5 -> [2]
      6 -> [3]
      7 -> [5,6]
      8 -> [4]
      9 -> []
      )))
   }

graph2 :: GraphBack Int Int
graph2 = GraphBack {
   getAllNodes = return [2,6,7,9,10,11],
   getKey = (\ i -> return (Just i)),
   getParents = (\ i -> return (Just (case i of
      2 -> []
      6 -> [9]
      7 -> [2,10]
      9 -> []
      10 -> [6]
      11 -> [6]
      )))
   }

graph3 :: GraphBack Int Int
graph3 = GraphBack {
   getAllNodes = return [1,2,3,4,5],
   getKey = (\ i -> return (Just i)),
   getParents = (\ i -> return (Just (case i of
      1 -> []
      2 -> [1]
      3 -> [1]
      4 -> [2,3]
      5 -> [4]
      )))
   }

graph4 :: GraphBack Int Int
graph4 = GraphBack {
   getAllNodes = return [1],
   getKey = (\ i -> return (Just i)),
   getParents = (\ i -> return (Just []))
   }

t34 :: IO ()
t34 =
   do
      l <- findCommonParents graph3 graph4 [4]
      putStrLn (show l)
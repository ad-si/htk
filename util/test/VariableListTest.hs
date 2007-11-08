-- | Contains a sample ListDrawer for testing purposes
module VariableListTest where


import Object
import VariableList

debugListDrawer :: Show a => ListDrawer a Int
debugListDrawer =
   let
      newPos pOpt aOpt =
         do
            pos <- newInt
            putStrLn ("N"++show pOpt++show aOpt++"->"++show pos)
            return pos
      setPos pos aOpt =
         putStrLn ("S"++show pos++show aOpt)
      delPos pos = putStrLn ("D"++show pos)
      redraw = putStrLn "W"
   in
      ListDrawer {newPos = newPos,setPos = setPos,delPos = delPos,
         redraw = redraw}



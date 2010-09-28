
module Main (main) where

import HTk.Toplevel.HTk

main:: IO ()
main = do
         main <- initHTk [text "Different Reliefs"]

         f  <- newVBox main []
         pack f []

         f1 <- newHBox f []
         pack f1 [PadY 10]

         f2 <- newHBox f []
         pack f2 [PadY 10]

         l1 <- newLabel f1 [text "Groove", relief Groove,
                            borderwidth (mm 1), font bigfont]
         pack l1 [PadX 10]

         l2 <- newLabel f1 [text "Ridge", relief Ridge,
                            borderwidth (mm 1), font bigfont]
         pack l2 [PadX 10]

         l3 <- newLabel f2 [text "Sunken", relief Sunken,
                            borderwidth (mm 1), font bigfont]
         pack l3 [PadX 10]

         l4 <- newLabel f2 [text "Raised", relief Raised,
                             borderwidth (mm 1), font bigfont]
         pack l4 [PadX 10]

         finishHTk

       where bigfont=  xfont {family = Just Lucida, weight = Just Bold,
                              pixels = (Just 18)}

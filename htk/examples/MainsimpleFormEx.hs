{- Tests and demonstrates toolkit/SimpleForm.hs -}
module Main(main) where

import Char

import HTk(withdrawWish)
import SimpleForm

data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
   deriving (Bounded,Enum,Show)

main =
-- Name/Address/E-mail/Age parser
   do
      withdrawWish

      let
         nonEmpty = any (not . isSpace)
         hasAt = elem '@'
         name =
            guardForm nonEmpty "Name must be non-empty" 
               (newFormEntry "Name" "")
         email =
            guardForm hasAt "E-mail must contain an @"
               (newFormEntry "E-mail" "") 


         colour = 
            mapForm
               (\ radColour ->
                  case radColour of
                     NoRadio -> Left "No colour specified"
                     Radio (col :: Colour) -> Right col
                  )
               (newFormEntry "Favorite Colour" NoRadio)
         form =
            name //
            newFormEntry "Address1" "" //
            newFormEntry "Address2" "" //
            email //
            newFormEntry "Age" (0::Int) //
            colour

      valueOpt <- doForm "Simple Form Test 1" form
      putStrLn (show valueOpt)
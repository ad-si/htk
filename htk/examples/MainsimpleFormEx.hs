-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

{- Tests and demonstrates toolkit/SimpleForm.hs -}
module Main(main) where

import Char

import HTk(withdrawWish)
import SimpleForm
import qualified Configuration

data Colour = Red | Orange | Yellow | Green | Blue | Violet
-- Indigo not a valid colour name!
   deriving (Bounded,Enum,Show)

instance HasConfigRadioButton Colour where
   configRadioButton colour = Configuration.background (show colour)

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
            colour //
            newFormEntry "British Subject" False

      valueOpt <- doForm "Simple Form Test 1" form
      putStrLn (show valueOpt)

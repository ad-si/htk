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

import qualified Configuration

import HTk(withdrawWish)
import SimpleForm
import MenuType
import HTkMenu

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

         language = HTkMenu(
            Menu
               "Favorite Language" [
               Button "English" 1,
               Button "German" 2,
               Blank,
               Button "Spanish" 3,
               Button "French" 4,
               Blank,
               Button "Hungarian" 5
               ])

         form =
            name //
            newFormEntry "Address1" "" //
            newFormEntry "Address2" "" //
            email //
            newFormEntry "Age" (0::Int) //
            colour //

            newFormEntry "British Subject" False //
            newFormMenu EmptyLabel language 

      valueOpt <- doForm "Simple Form Test 1" form
      putStrLn (show valueOpt)
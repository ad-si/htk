{- Tests and demonstrates toolkit/SimpleForm.hs -}
module Main(main) where

import Char

import HTk(withdrawWish)
import SimpleForm

main =
-- Name/Address/E-mail/Age parser
   do
      withdrawWish

      let
         nonEmpty = any (not . isSpace)
         hasAt = any (== '@')
         name =
            guardForm nonEmpty "Name must be non-empty" 
               (newFormEntry "Name" "")
         email =
            guardForm hasAt "E-mail must contain an @"
               (newFormEntry "E-mail" "")

         form =
            name //
            newFormEntry "Address1" "" //
            newFormEntry "Address2" "" //
            email //
            newFormEntry "Age" (0::Int)
      valueOpt <- doForm "Simple Form Test 1" form
      putStrLn (show valueOpt)
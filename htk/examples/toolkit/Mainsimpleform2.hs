{- Test of doFormList -}
module Main where

import Util.Computation

import Events.Events

import HTk.Toolkit.SimpleForm

main =
   do
      let
         form1 :: Form String
         form1 = newFormEntry "Entry" "Default"

         form2a :: Form Int
         form2a = newFormEntry "Number" 23

         form2 :: Form String
         form2 = fmap show form2a

         form3 :: Form String
         form3 = fmap (const "EMPTY") emptyForm

      (event,destroy) <- doFormList "Form sequence"
         [(form1,"String"),(form2,"Number"),(form3,"Nowt")]

      forever (
         do
            valWE <- sync event
            putStrLn (show (fromWithError valWE))
         )

{- A simple menu example, using the HTkMenu interface -}
module Main(main) where

import Computation

import Events
import Destructible

import HTk hiding (Menu)
import MenuType
import HTkMenu

main :: IO ()
main =
   do
      top <- initHTk [text "Menu"]
      let
         (simpleMenu :: HTkMenu Char) =
            HTkMenu(
               Menu "Names" [
                  Button "Ariel" 'A',
                  Button "Bendickt" 'B',
                  Button "Cordelia" 'C',
                  Button "Duncan" 'D',
                  Button "Quit" 'Q'
                  ])

      (menuButton,event) <- compileHTkMenu top simpleMenu 
      pack menuButton []

      let
         eventHandler =
            do
               ch <- event
               always (putStrLn ("Got "++[ch]))
               case ch of
                  'Q' -> done
                  _ -> eventHandler
      sync eventHandler
      cleanupWish
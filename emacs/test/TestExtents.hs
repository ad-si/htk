module Main(main) where

import Events.Events
import Events.Destructible

import Emacs.Basic
import Emacs.Extents

main :: IO ()
main =
   do
      s <- newEmacsSession "TestBuffer"
      initBuffer s
      addContainerBuffer s "c1"
      addContainer s "c1" "c1.1"
      addText s "c1.1" "Editable1"
      addButton s "c1.1" "b1.1" "Click me1"
      boundContainer s "c1.1"
      addButton s "c1" "b1" "Click me2"
      addText s "c1" "Editable2"
      addButton s "c1" "b2" "Quit"
      boundContainer s "c1"
      let
         ev =
               (do
                  emacsEvent s "b1.1"
                  always (putStrLn "b1.1 clicked")
                  ev
               )
            +> (do
                  emacsEvent s "b1"
                  always (putStrLn "b1 clicked")
                  ev
               )
            +> emacsEvent s "b2"
      sync ev
      contents1 <- containerContents s "c1"
      contents11 <- containerContents s "c1.1"
      putStrLn (show contents1)
      putStrLn (show contents11)
      destroy s

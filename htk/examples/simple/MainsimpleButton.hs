{- "Goodbye" demo as discussed on the Haskell mailing list.
   Specification: pops up a window saying "Hello World" and presenting a
   button saying "Bye".  Clicking the button once changes the message to
   "Goodbye"; clicking it a second time causes the program to quit.
   -}
module Main where

import Util.Computation

import Events.Events

import HTk.Toplevel.HTk

main =
   do
      mainWin <- initHTk [text "Hello World"]
      label <- newLabel mainWin [text "Hello World"]
      button <- newButton mainWin [text "Bye"]
      pack label []
      pack button []
      buttonClicked <- clicked button
      sync buttonClicked
      label # text "Goodbye"
      sync buttonClicked
      destroy mainWin


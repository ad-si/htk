module Main (main) where

import HTk.Toplevel.HTk
main :: IO ()
main =
  do main <- initHTk [text "A Listbox"]
     lb  <- newListBox main [value numbers, bg "white",
                            size (15, 10)] :: IO (ListBox String)
     pack lb [Side AtLeft]
     scb <- newScrollBar main []
     pack scb [Side AtRight, Fill Y]
     lb # scrollbar Vertical scb
     (press, _) <- bindSimple lb (ButtonPress (Just 1))
     lb # selectMode Extended
     spawnEvent (forever
       (press >> always
          (do sel<- getSelection lb
              putStrLn ("Selected "++
                        show (sel:: Maybe [Int])))))
     finishHTk where
  numbers =
    ["One", "Two", "Three", "Four", "Five", "Six", "Seven",
     "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen",
     "Fourteen", "Fifteen", "Sixteen", "Seventeen",
     "Eighteen", "Nineteen", "Twenty"]


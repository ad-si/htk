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

module Main (main) where

import HTk

main :: IO ()
main =
  do
    main <- initHTk [text "A Listbox"]
    lb  <- newListBox main [value numbers, bg "white", size (15, 10)] ::
             IO (ListBox String)
    pack lb [Side AtLeft]
    scb <- newScrollBar main []
    pack scb [Side AtRight, Fill Y]
    lb # scrollbar Vertical scb
    (press, _) <- bindSimple lb (ButtonPress (Just (BNo 1)))
    spawnEvent (forever
                  (press >> always (do
                                      sel<- getSelection lb;
				      putStrLn ("Selected "++ 
					        show (sel::
                                                        Maybe [Int])))))
    finishHTk

  where numbers =
          ["One", "Two", "Three", "Four", "Five", "Six", "Seven",
           "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirtheen",
           "Fourteen", "Fifteen", "Sixteen", "Seventeen",
           "Eighteen", "Nineteen", "Twenty"]

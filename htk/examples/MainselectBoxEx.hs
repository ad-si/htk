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
import SelectBox

main:: IO ()
main =
  do
    main <- initHTk [text "select box example"]

    sb1 <- newSelectBox main Nothing []
    pack sb1 [Fill X, Expand On, Side AtTop]

    b1_1 <- addButton sb1 [text "Button1.1"] [] :: IO (Button String)
    b2_1 <- addButton sb1 [text "Button2.1"] [] :: IO (Button String)
    addSpace sb1 10
    b3_1 <- addButton sb1 [text "Button3.1"] [] :: IO (Button String)
    b4_1 <- addButton sb1 [text "Button4.1"] [] :: IO (Button String)

    sb2 <- newSelectBox main (Just 2) []
    pack sb2 [Fill X, Expand On, Side AtBottom]

    b1_2 <- addButton sb2 [text "Button1.2"] [] :: IO (Button String)
    b2_2 <- addButton sb2 [text "Default Button"] [] :: IO (Button String)
    b3_2 <- addButton sb2 [text "Button3.2"] [] :: IO (Button String)
    b4_2 <- addButton sb2 [text "Button4.2"] [] :: IO (Button String)

    finishHTk


module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.SelectBox

main:: IO ()
main =
  do
    main <- initHTk [text "select box example"]

    sb1 <- newSelectBox main Nothing []
    pack sb1 [Fill X, Expand On, Side AtTop]

    b1_1 <- addButton sb1 [text "Button1.1"] []
    b2_1 <- addButton sb1 [text "Button2.1"] []
    addSpace sb1 10
    b3_1 <- addButton sb1 [text "Button3.1"] []
    b4_1 <- addButton sb1 [text "Button4.1"] []

    sb2 <- newSelectBox main (Just 2) []
    pack sb2 [Fill X, Expand On, Side AtBottom]

    b1_2 <- addButton sb2 [text "Button1.2"] []
    b2_2 <- addButton sb2 [text "Default Button"] []
    b3_2 <- addButton sb2 [text "Button3.2"] []
    b4_2 <- addButton sb2 [text "Button4.2"] []

    finishHTk

-- Delayed "Hello World" program.  This is to test that initHTk doesn't
-- put up a window until we want it to.
module Main (main) where

import HTk.Toplevel.HTk

main:: IO ()
main =
  do
    main <- initHTk []

    putStrLn "Press Enter to display window"
    getLine

    l <- newLabel main [text "Hello world!"]
    pack l []
    finishHTk

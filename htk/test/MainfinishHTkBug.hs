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
      
main:: IO ()
main =
  do main <- initHTk []

     l  <- newLabel main [text "Hello"]
     pack l []

     nb <- newButton main [text "destroy label"] :: IO (Button String)
     pack nb []
     clickednb <- clicked nb
     spawnEvent (clickednb >>> destroy l)

     (enter_main, _) <- bindSimple main Enter
     (enter_nb, _) <- bindSimple nb Enter

     spawnEvent (forever (enter_main >>> putStrLn "entered main" +>
                          enter_nb >>> putStrLn "entered nb"))
     finishHTk

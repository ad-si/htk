{- --------------------------------------------------------------------
 -
 - HTk Examples: User interactions
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

import HTk
import Label
import Button
import Mouse
import UserInteraction

main :: IO ()
main =
  do
    tk <- htk []
    main <- newVBox []
    win <- window main [text "user interactions test"]
    labels <- newHBox [parent main]
    lab1 <- newLabel [size (30, 10), bg "green", value "outside 1",
                      parent labels]
    lab2 <- newLabel [size (30, 10), bg "green", value "outside 2",
                      parent labels]
    lab3 <- newLabel [size (30, 10), bg "green", value "outside 3",
                      parent labels]
    userInteraction (\i -> (mouseEvent' lab1 Enter >>>
                              ((lab1 # value "inside 1") >>
                               (lab1 # bg "red") >> done)) +>
                           (mouseEvent' lab1 Leave >>>
                               ((lab1 # value "outside 1") >> 
                                (lab1 # bg "green") >> done)) +>
                           (mouseEvent' lab2 Enter >>>
                              ((lab2 # value "inside 2") >>
                               (lab2 # bg "red") >> done)) +>
                           (mouseEvent' lab2 Leave >>>
                               ((lab2 # value "outside 2") >> 
                                (lab2 # bg "green") >> done)) +>
                           (mouseEvent' lab3 Enter >>>
                              ((lab3 # value "inside 3") >>
                               (lab3 # bg "red") >> done)) +>
                           (mouseEvent' lab3 Leave >>>
                               ((lab3 # value "outside 3") >> 
                                (lab3 # bg "green") >> done)))
    buttons <- newHBox [parent main]
    destr1 <- newButton [width 30, text "destroy label 1", parent buttons,
    	                 command (\ ()-> return ())]
    destr2 <- newButton [width 30, text "destroy label 2", parent buttons,
                         command (\ () -> destroy lab2)]
    destr3 <- newButton [width 30, text "destroy label 3", parent buttons,
                         command (\ () -> destroy lab3)]
    interactor (\i -> triggered destr1 >>> destroy lab1 +> triggered destr2 +> triggered destr3)
    sync(destroyed win)
    destroy tk

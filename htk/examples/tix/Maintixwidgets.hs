
module Main (main) where

import HTk.Toplevel.HTk

main:: IO ()
main =
  do
    main <- initHTk [text "tix widget example!"]
    requirePackage "Tix"
    tix  <- isTixAvailable

    (if tix then

       do
         nb <- newNoteBook main [size (300, 300)]
         pack nb [PadX 10, PadY 10, Fill X, Expand On]

         page1 <- createNoteBookPage nb "Entries" []
         page2 <- createNoteBookPage nb "Info" []

         lf_pers <- newLabelFrame page1 [text "Personal"]
         grid lf_pers [GridPos (0,0), Sticky NSEW]

         l1  <- newLabel lf_pers [text "Name:", tooltip "enter your name"]
         grid l1 [GridPos (0,0), GridPadX 5, GridPadY 5]

         e1 <- newEntry lf_pers
                        [tooltip "enter your name"] :: IO (Entry String)
         grid e1 [GridPos (1,0), GridPadX 5, GridPadY 5]


         l2 <- newLabel lf_pers [text "Age:", tooltip "enter your age"]
         grid l2 [GridPos (0,1), GridPadX 5, GridPadY 5]

         e2 <- newEntry lf_pers
                        [tooltip "enter your age"] :: IO (Entry String)
         grid e2 [GridPos (1,1), GridPadX 5, GridPadY 5]

         (cb1 :: ComboBox String) <- newComboBox lf_pers True
                                       [value ["male", "female"], pick 0]
         grid cb1 [GridPos (0,2), GridPadX 5, GridPadY 5, Columnspan 2]

         lf_web <- newLabelFrame page1 [text "WWW"]
         grid lf_web [GridPos (0,1), Sticky NSEW]

         l3  <- newLabel lf_web [text "Email:",
                                 tooltip "enter your email"]
         grid l3 [GridPos (0,0), GridPadX 5, GridPadY 5]

         e3 <- newEntry lf_web
                        [tooltip "enter your email"] :: IO (Entry String)
         grid e3 [GridPos (1,0), GridPadX 5, GridPadY 5]

         l4 <- newLabel lf_web [text "Homepage:",
                                tooltip "enter your homepage"]
         grid l4 [GridPos (0,1), GridPadX 5, GridPadY 5]

         e4 <- newEntry lf_web [tooltip "enter your homepage"]
                 :: IO (Entry String)
         grid e4 [GridPos (1,1), GridPadX 5, GridPadY 5]

         lf_inf <- newLabelFrame page2 [text "Info"]
         grid lf_inf [GridPos (0,0), Sticky NSEW]

         inf2 <- newMessage lf_inf [text "This is an example for widgets, that are only available with tixwish.\nYou can get information about which wish you are using via the tixAvailbale flag.", tooltip "tix widget example info"]
         grid inf2 [GridPos (0,0), Sticky NSEW]

         finishHTk

     else

       do
         lab <- newLabel main [text "Sorry, only for use with Tix!"]
         pack lab [PadX 20, PadY 10]

         quit <- newButton main [text " Quit "]
         pack quit [PadY 10]
         clickedquit <- clicked quit
         spawnEvent (clickedquit >>> destroy main)

         finishHTk)

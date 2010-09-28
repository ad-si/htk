
module Main(main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.ModalDialog
import HTk.Toolkit.DialogWin
import HTk.Toolkit.MarkupText
import HTk.Toolkit.InputWin
import HTk.Toolkit.InputForm
import HTk.Toolkit.TextDisplay

data Test = Test {ent1 :: String, ent2 :: String, enu1 :: Int, ent3 :: Int, chck1 :: Bool} deriving Show

main :: IO ()
main =
 do
  htk <- initHTk[text "main window"]
  but1 <- newButton htk [text "Modal Dialogs"]
  but4 <- newButton htk [text "Non-modal dialogs"]
  but5 <- newButton htk [text "Alerts"]
  but6 <- newButton htk [text "Errors"]
  but7 <- newButton htk [text "Warnings"]
  but8 <- newButton htk [text "Confirmation"]
  but8a <- newButton htk [text "Information"]
  but9 <- newButton htk [text "Input window"]
  but2 <- newButton htk [text " Quit example "]
  but35 <- newButton htk [text "Simple text display"]

  pack but1 []
  pack but4 []
  pack but35 []
  pack but5 []
  pack but6 []
  pack but7 []
  pack but8 []
  pack but8a []
  pack but9 []
  pack but2 []

  clickedbut1 <- clicked but1
  spawnEvent (forever (clickedbut1 >>>
    (do tp <- createToplevel [text "ModalDialog"]
        but3 <- newButton tp [text "Ok"]
        pack but3 []
        clickedbut3 <- clicked but3
        test <- modalDialog tp True (clickedbut3 >>
                                     (always (return "ModalDialogOk")))
        putStrLn test)))

  clickedbut35 <- clicked but35
  spawnEvent (forever (clickedbut35 >>>
    (do f  <- readFile "/etc/passwd"
        createTextDisplay "Display /etc/passwd" f [size (50,10)] )))


  clickedbut4 <- clicked but4
  spawnEvent (forever (clickedbut4 >>>
    (do tp <- createToplevel [text "nonModalDialog"]
        but3 <- newButton tp [text "Ok"]
        pack but3 []
        clickedbut3 <- clicked but3
        test <- modalDialog tp False (clickedbut3 >>>
                                       (return "nonModalDialogOk"))
        putStrLn test)))

  clickedbut5 <- clicked but5

  spawnEvent (forever (clickedbut5 >>>
    createAlertWin "Your printer is on fire!" []))

  clickedbut6 <- clicked but6
  spawnEvent (forever (clickedbut6 >>>
    createErrorWin "Segmentation violation.\nCore dumped." []))

  clickedbut7 <- clicked but7
  spawnEvent (forever (clickedbut7 >>>
    createWarningWin ("Please extinguish all cigarettes and switch off \n"
       ++ "all mobile phones.") []))

  clickedbut8 <- clicked but8
  spawnEvent (forever (clickedbut8 >>>
    (do res <- createConfirmWin "Really delete all files?" []
        putStrLn ("Result of confirmation: "++ show res))))

  clickedbut8a <- clicked but8a
  spawnEvent (forever (clickedbut8a >>>
    createMessageWin "This message will self-destruct in 10 seconds." []))

  clickedbut9 <- clicked but9
  spawnEvent (forever (clickedbut9 >>>
    (do -- one adt to store the entered information and
        -- the initial field values
        let def = Test{ent1="HALLO WELT!", ent2="TEST", enu1=5, ent3=42,
                       chck1=True}
        -- create the InputForm (as a function so there is no parent)
        -- give as value (Just a) or Nothing
        let iform p = newInputForm p (Just def) []
        -- create the InputWindow with the formfunction
        -- returns the InputWindow and the InputForm (which can be filled now)
        (iwin,form) <- createInputWin "Please enter all relevant data below." iform []
        -- add various fields here to the InputForm
        newTextField form [size (5,5), selector ent1, text "Editor String",
                           modifier (\ old val -> old {ent1=val})]
                                         :: IO (TextField Test String)
        l<- newLabel form [text "This is some explanatory text."]
        pack l [PadX (mm 5), Side AtLeft]
        newEntryField form [text "Entry Int", selector ent3,
                            modifier (\ old val -> (old {ent3=val}))]
                                         :: IO (EntryField Test Int)
        newEnumField form [0,1,2,3,4,5]
          [text "Option Int", selector enu1,
           modifier (\ old val -> old {enu1=val})] :: IO (EnumField Test Int)
        newCheckboxField form True [text "The above is true: ",
                                    selector chck1, modifier (\o v-> o{chck1= v})]
        -- wait for user input
        res <- wait iwin True
        case res of
           Nothing -> putStrLn "Cancelled!"
           Just val -> putStrLn("Result: "++ show val))))

  clickedbut2 <- clicked but2
  spawnEvent (forever (clickedbut2 >>> destroy htk)) -- game over.
  finishHTk

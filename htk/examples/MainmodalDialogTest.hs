module Main(main) where

import HTk
import ModalDialog
import DialogWin

main :: IO ()
main =
 do
  htk <- initHTk[text "main window"]
  but1 <- newButton htk [text "create ModalDialog"] :: IO (Button String)
  but4 <- newButton htk [text "create nonModalDialog"] :: IO (Button String)
  but5 <- newButton htk [text "create newAlertWin"] :: IO (Button String)
  but6 <- newButton htk [text "create newErrorWin"] :: IO (Button String)
  but7 <- newButton htk [text "create newWarningWin"] :: IO (Button String)
  but8 <- newButton htk [text "create newConfirmWin"] :: IO (Button String)
  but2 <- newButton htk [text " Quit example "] :: IO (Button String)
  

  pack but1 []
  pack but4 []
  pack but5 []  
  pack but6 []  
  pack but7 []  
  pack but8 []  
  pack but2 []

  
  clickedbut1 <- clicked but1
  spawnEvent (forever (clickedbut1 >> always (do 
                                               tp <- createToplevel [text "ModalDialog"]
                                               but3 <- newButton tp [text "Ok"] :: IO (Button String)
                                               pack but3 []    
                                               clickedbut3 <- clicked but3
                                               test <- modalDialog tp True (clickedbut3 >> (always (return "ModalDialogOk")))
					       putStrLn test
					       )))
  clickedbut4 <- clicked but4
  spawnEvent (forever (clickedbut4 >> always (do 
                                               tp <- createToplevel [text "nonModalDialog"]
                                               but3 <- newButton tp [text "Ok"] :: IO (Button String)
                                               pack but3 []    
                                               clickedbut3 <- clicked but3
                                               test <- modalDialog tp False (clickedbut3 >> (always (return "nonModalDialogOk")))
					       putStrLn test
					       )))
 
  clickedbut5 <- clicked but5
  spawnEvent (forever (clickedbut5 >> always (do 
                                               newAlertWin "AlertWin test" []  
					       putStrLn "done with AlertWin"
					       )))
 
  clickedbut6 <- clicked but6
  spawnEvent (forever (clickedbut6 >> always (do 
                                               newErrorWin "ErrorWin test" []  
					       putStrLn "done with ErrorWin"
					       )))
 
  clickedbut7 <- clicked but7
  spawnEvent (forever (clickedbut7 >> always (do 
                                               newWarningWin "WarningWin test" []  
					       putStrLn "done with WarningWin"
					       )))
 
  clickedbut8 <- clicked but8
  spawnEvent (forever (clickedbut8 >> always (do 
                                               res <- newConfirmWin "ConfirmWin test" []  
					       putStr "done with ConfirmWinWin: "
					       putStrLn (show res)
					       )))
 
  clickedbut2 <- clicked but2
  spawnEvent (forever (clickedbut2 >> always (destroy htk)))
 
  (htk_destr, _) <- bindSimple htk Destroy
  sync (htk_destr)



module Main(main) where

import HTk
import ModalDialog

main :: IO ()
main =
 do
  htk <- initHTk[text "main window"]
  but1 <- newButton htk [text "create ModalDialog"] :: IO (Button String)
  but4 <- newButton htk [text "create nonModalDialog"] :: IO (Button String)
  but2 <- newButton htk [text " Quit example "] :: IO (Button String)
  

  pack but1 []
  pack but4 []
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
 
  clickedbut2 <- clicked but2
  spawnEvent (forever (clickedbut2 >> always (destroy htk)))
 
  (htk_destr, _) <- bindSimple htk Destroy
  sync (htk_destr)



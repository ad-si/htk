module Main(main) where

import HTk
import ModalDialog
import DialogWin
import MarkupText
import InputWin
import InputForm

data Test = Test {ent1 :: String, ent2 :: String, enu1 :: Int, ent3 :: Int}  

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
  but9 <- newButton htk [text "create InputWin"] :: IO (Button String)
  but2 <- newButton htk [text " Quit example "] :: IO (Button String)
  

  pack but1 []
  pack but4 []
  pack but5 []  
  pack but6 []  
  pack but7 []  
  pack but8 []  
  pack but9 []  
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
                                               newAlertWin [bold [prose "An Alert Window has been triggered."], newline, 
                                                            prose "This would be the place to put some warning or so!"] []  
					       putStrLn "done with AlertWin"
					       )))
 
  clickedbut6 <- clicked but6
  spawnEvent (forever (clickedbut6 >> always (do 
                                               newErrorWin [bold [prose "An Error Window has been triggered."], newline,
                                                            prose "This would be the place where to find the error message!"] []  
					       putStrLn "done with ErrorWin"
					       )))
 
  clickedbut7 <- clicked but7
  spawnEvent (forever (clickedbut7 >> always (do 
                                               newWarningWin [bold [prose "A Warning Window has been triggered."], newline,
                                                              prose "This text here could be a warning!"] []
					       putStrLn "done with WarningWin"
					       )))
 
  clickedbut8 <- clicked but8
  spawnEvent (forever (clickedbut8 >> always (do 
                                               res <- newConfirmWin [bold [prose "A Confirm Window has been triggered."], newline,
                                                                     prose "Here the action to be confirmed would be found!"] []  
					       putStr "done with ConfirmWinWin: "
					       putStrLn (show res)
					       )))

  clickedbut9 <- clicked but9
  spawnEvent (forever (clickedbut9 >> always (do 
                                               let def = Test{ent1="HALLO WELT!\n rofl", ent2="TEST", enu1=5, ent3=42}
                                               iwin <- newInputWin "HELLO THERE" (Just def) []
					       newTextField (iwin # fForm) [size (5,5),selector ent1, 
					                           modifier (\ old val -> old {ent1=val})] :: IO (TextField Test String)
					       newEntryField (iwin # fForm) [text "Entry Int", selector ent3, modifier (\ old val -> (old {ent3=val}))] :: IO (EntryField Test Int)
					       newEnumField (iwin # fForm) [0,1,2,3,4,5] [selector enu1, modifier (\ old val -> old {enu1=val})] :: IO (EnumField Test Int)
					       res <- wait iwin True
                                               case res of
					        Nothing -> putStrLn "canceled"
						Just val -> putStrLn(show(val # ent1)++" "++show(val # ent3)++" "++show(val # enu1))
					       )))
 
  clickedbut2 <- clicked but2
  spawnEvent (forever (clickedbut2 >> always (destroy htk)))
 
  (htk_destr, _) <- bindSimple htk Destroy
  sync (htk_destr)



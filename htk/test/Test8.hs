{- #########################################################################

MODULE        : TestManual
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget



   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import SIM

import Label
import Editor
import InputForm
import InputWin
import ScrollBox
import LabelBox
import ComboBox
import PulldownMenu
import Entry
import SelectBox
import IconBar
import BitMap
import Bell

import DialogWin
import PromptWin

import Directory
import Debug(debug)


main = do
        mkEntry
        mkTextEdit
        mkScrollBox
        mkComboBox
        mkLabelBox
        mkScrollBox
        mkInputForm1
        mkOptMenu
        mkSelectBox
        mkIconBar

        mkInputForm2
        mkAlertWin
        mkPromptWin
        mkConfirmWin

        mkCascadeMenu

        done


mkEntry = do
        b <- newVBox []
        newLabel [value "",title "hello",parent b]
        newLabel [value (1::Int),title (2::Int),parent b]
        win <- window b []
        controller win (const inaction)


title :: (GUIObject w, GUIValue v) => v -> Config w
title v w = cset w "text" v

mkTextEdit = do
   htk [text "Test"]
   ed <- newEditor [bg "white", size (30,15),flexible]
   win <- window ed [text "Text Editor"]
   interactor (\iact ->
        destroyed win >>> do {shutdown; stop iact}
        )
   ed # font (Helvetica,12::Int)
   ed # value [0..100::Int]
   done

instance GUIValue [Int] where
        cdefault = []


mkScrollBox = do
        t <- newEditor [size (10,10), bg "white",flexible]
        sb <- newScrollBox (t::Editor String) [flexible]
        win <- window sb [text "ScrollBox"]
        controller' win inaction        

mkLabelBox = do
        b <- newVFBox [flexible]
        e <- newEntry [value (0::Int), width 5]
        lb <- newLabelBox e [text "Count: ",orient Horizontal,parent b]
        win <- window b [text "LabelBox"]
        controller' win (inaction)      


mkComboBox = do 
        dir <- getCurrentDirectory
        fnms <- getDirectoryContents dir
        b <- newComboBox [flexible]
        configure (fListBox b) [value fnms,bg "white"]
        win <- window (b::ComboBox String) [text "ComboBox"]
        controller' win (receive b >>>= startEditor)
 where  startEditor :: String -> IO ()
        startEditor fnm = do
                t <- newEditor [bg "white"]
                s <- newScrollBox (t::Editor String) []
                win <- window s [text ("Text Editor: " ++ fnm)]
                readTextFromFile t fnm
                controller' win inaction
                done


data HaskellKind = Plain | Literal deriving (Eq,Ord,Enum,Read,Show)

instance GUIValue HaskellKind where
        cdefault = Plain

mkOptMenu = do
        mb <- optmenu [Plain,Literal]
        win <- window mb [text "Option Menu"]
        controller' win (receive mb >>> done)
 where  optmenu :: [HaskellKind] -> IO (MenuButton HaskellKind)         
        optmenu il = do
                mb <- newMenuButton [
                        indicator On, 
                        relief Raised,
                        borderwidth 1,
                        flexible,
                        text (show Plain)
                        ]
                mn <- newPulldownMenu mb [tearOff On]
                mapM (\i -> newButton [text (show i),cmd mb i,parent mn]) il
                return mb
        cmd :: MenuButton HaskellKind -> HaskellKind -> Config (Button HaskellKind)
        cmd mb i = command (\() -> do {text (show i) mb; return i})
        

data Point = Point {fX :: Int,  fY :: Int} deriving (Eq,Read,Show)

mkInputForm1 = do {
        form <- createForm;
        win <- window form [text "Input Form"];
        controller win (const inaction);
        done
} 

mkInputForm2 = do {
        form <- createForm;
        forkIO(newInputWin "Enter Point" form Nothing [] >> done);
        done
} 


createForm = do {
        form <- newInputForm [width (cm 20), flexible, value (Point 1 10)];
        newEntryField [text "X:",
                selector fX,
                modifier (\r x -> r{fX = x}),
                bg "white",
                parent form, 
                width 5
                ];
        newEntryField [text "Y:",
                selector fY,
                modifier (\r y -> r{fY = y}),
                bg "white",
                parent form, 
                width 5
                ];
        return form
} 



mkSelectBox = do 
        sb <- newSelectBox (Just 1) [flexible]
        newButton [command (\() -> return True), text "Ok",parent sb]
        newButton [command (\() -> return False), text "Cancel",parent sb]
        win <- window (sb::SelectBox Bool) [text "SelectBox"]
        controller win (const (receive sb))
        done


mkIconBar = do {
        ib <- newIconBar [orient Horizontal,flexible];
        newButton [parent ib,
                command (\() -> return "i3"),
                bitmap "icons/ms_down_arrow.bm"
                ];
        newButton [parent ib,
                command (\() -> return "i4"),
                bitmap "icons/ms_up_arrow.bm"
                ];
        newButton [parent ib,
                command (\() -> return "i5"),
                bitmap "icons/ms_left_arrow.bm"
                ];
        newButton [parent ib,
                command (\() -> return "i6"),
                bitmap "icons/ms_right_arrow.bm"
                ];
        bts <- getIconButtons ib;
        foreach bts (\eib -> configure eib [width (cm 1), height (cm 1)]);
        win <- window (ib :: IconBar String) [text "IconBar"];
        controller' win (receive ib >>> bell )
        }

mkAlertWin =
        forkIO (newWarningWin "Transaction Completed" [] >> done)

mkConfirmWin =
        forkIO (newConfirmWin "File has been edited\nDo You really want to quit?" [] >> done)

mkPromptWin =
        forkIO (newPromptWin "Enter File Name" "" [] >> done)


mkCascadeMenu = do
        mb <- newMenuButton [text "Menu"]
        mn <- newHaskFileMenu
        mn # parent (mb :: MenuButton ())
        win <- window mb [text "button"]
        controller win (const (inaction))




newVersionFileMenu ::Menu () -> IO (Menu ())
newVersionFileMenu mn = do
   cmn <- newCascadeMenu [text "Version",parent mn]
   newMenuItem (cmn::Menu ()) [text "New Branch"]
   newMenuItem cmn [text "Prune"]
   newMenuItem cmn [text "Set Default"]
   return cmn


newExportImportFileMenu :: Menu () -> IO (Menu ())
newExportImportFileMenu mn = do {
        cmn <- newCascadeMenu [
                text "Export/Import", 
                parent mn];
        newMenuItem (cmn :: Menu ()) [
                text "Export"
                ];
        newMenuItem cmn [
                text "Import"
                ];
        newMenuItem cmn [
                text "Check-Out"];
        newMenuItem cmn [
                text "Check-In"
                ];
        newMenuItem cmn [
                text "Cleanup"
                ];
        return cmn
        }


newHaskFileMenu :: IO (Menu ())
newHaskFileMenu = do
   mn <- newMenu []
   newMenuItem mn [text "Edit File"]
   newMenuItem mn [ text "Edit Attributes"]
   newMenuItem mn [text "Interpret"]
   newVersionFileMenu mn
   newExportImportFileMenu mn
   return mn

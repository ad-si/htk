{- #########################################################################

MODULE        : TestManual
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Entry Widget

                Scrollbox - colours, slider config options

                Space - fill directives not overwritten by box


   ######################################################################### -}


module Main (
        main

        ) where

import HTk
import SIM
-- import Dynamics

import ScrollBox
import LabelBox

import Label
import Entry
import Message
import Separator
import Button
import CheckButton
import RadioButton
import MenuButton
import PulldownMenu
import RadioGroup
import SpinButton
import Scale
import Frame
import ListBox
import ScrollBar
import Space
import ComboBox
import ScrollBox
import Editor
import PromptWin

import DialogWin

import Colour
import Keyboard
import Mouse
import Bell
import Index

import ExtendedPrelude
import Directory

import InputForm
import InputWin

import TestUtil (logout)
import Debug(debug)


main = 
   do
      htk [{- logfile (1::Int) -}]
      setErrorHandler errWin
      newTextEditor
      mkComboBox
      mkOptMenu
      mkHaskellForm
      block

errWin e = 
        newErrorWin ("Haskell-Tk error occured: " ++ show e)[modal True]

-- ---------------------------------------------------------------------------
-- Building the Text Editor
-- ---------------------------------------------------------------------------

newTextEditor = do
        b <- newVBox [flexible]
        mb <- newMenuButton [text "File",parent b,anchor West]  
        mn <- newEditorMenu
        configure mn [parent (mb::MenuButton EditorCmd)]
        ed <- newEditor [size (60,30), bg "white",flexible,value ""]
        sb <- newScrollBox ed [flexible,parent b]
        win <- window b [text "Text Editor"]
        controller' win (receive mn >>>= \f -> f ed)    

-- ---------------------------------------------------------------------------
-- Building the Menu
-- ---------------------------------------------------------------------------

type EditorCmd = Editor String -> IO ()

newEditorMenu :: IO (Menu EditorCmd)
newEditorMenu = do
        mn <- newMenu []
        newButton [text "Open...", cmd openFile,  parent mn]
        newButton [text "Save...", cmd saveFile,  parent mn]
        newButton [text "Empty", cmd emptyEditor, parent mn]
        return mn
 where  cmd f = command (\() -> return f)

-- ---------------------------------------------------------------------------
-- Semantic Actions
-- ---------------------------------------------------------------------------

openFile :: Editor String -> IO ()
openFile ed    = withFile (readTextFromFile ed)

saveFile :: Editor String -> IO ()
saveFile ed    = withFile (writeTextToFile ed)

emptyEditor :: Editor String -> IO ()
emptyEditor ed = do {configure ed [value ""]; done}

withFile       = withPrompt "Enter File Name"

withPrompt :: GUIValue a => String -> (a -> IO ()) -> IO ()
withPrompt msg f =  do
        ans <- newPromptWin msg cdefault [modal True]
        incase ans f





mkComboBox = do 
        dir <- getCurrentDirectory
        fnms <- getDirectoryContents dir
        b <- newComboBox []
        configure (fListBox b) [value fnms]
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


-- ---------------------------------------------------------------------------
-- Option Menu
-- ---------------------------------------------------------------------------

mkOptMenu = do
        mb <- newOptionMenu ["red","yellow","green"]
        win <- window mb [text "Option Menu"]
        controller' win (receive mb >>>= flip bg mb)


data OptionMenu a = OptionMenu (MenuButton a) (Menu a) [Button a]

newOptionMenu :: GUIValue a => [a] -> IO (OptionMenu a)         
newOptionMenu il = do
        mb <- newMenuButton [ 
                indicator On,
                borderwidth 1,
                relief Raised, 
                text ((il++[cdefault])!!1)
                ]
        mn <- newPulldownMenu mb [tearOff On]
        bts <- mapM (newChoiceButton mb mn) il
        return (OptionMenu mb mn bts)
 where  newChoiceButton mb mn i = 
                newButton [text i,cmd mb i,parent mn]
        cmd :: GUIValue a => MenuButton a -> a -> Config (Button a)
        cmd mb i = command (\() -> do {text i mb; return i})
        text v w = cset w "text" v


instance GUIObject (OptionMenu a) where
        toGUIObject (OptionMenu mb _ _) = toGUIObject mb

instance HasColour (OptionMenu a) where
        setColour omn@(OptionMenu mb mn bts) cid v = 
                synchronize omn (do {
                        setColour mb cid v;
                        setColour mn cid v;
                        foreach bts (\bt -> setColour bt cid v);
                        return omn
                        })

instance Widget (OptionMenu a) where
        cursor c omn@(OptionMenu mb mn bts) = 
                synchronize omn (do {
                        cursor c mb;
                        cursor c mn;
                        foreach bts (cursor c);
                        return omn
                        })

instance ChildWidget (OptionMenu a)
instance GUIValue a => HasReceiveEV OptionMenu a where
        receive (OptionMenu mb mn bts) = receive mb
instance Synchronized (OptionMenu a) where
        synchronize (OptionMenu mb _ _) = synchronize mb
        


-- ---------------------------------------------------------------------------
-- Haskell Input Form
-- ---------------------------------------------------------------------------


data HaskellDescr =  HaskellDescr {fName :: String, fVersion :: Int, fKind :: HaskellKind, fText :: [String]}

data HaskellKind = Plain | Literal deriving (Eq,Ord,Show,Read)

instance GUIValue HaskellKind where
        cdefault = Plain

editHaskellDescr :: HaskellDescr -> IO (Maybe HaskellDescr)
editHaskellDescr hd = do
        form <- newInputForm [width (cm 20), flexible,value hd];
        newEntryField [text "Name:",
                selector fName,
                replacor (\r fName -> r{fName=fName}),
                parent form
                ]
        newEntryField [text "Version Id:",
                selector fVersion,
                replacor (\r fVersion -> r{fVersion=fVersion}),
                parent form
                ]
        newTextField [text "Description:",
                selector (fText),
                replacor (\r t -> r{fText = t}),
                parent form, 
                width 20,
                height 10
                ]
        newEnumField [Plain,Literal] [
                text "Haskell-Kind:", 
                selector fKind,
                replacor (\r fKind -> r{fKind=fKind}), 
                parent form
                ]
        newInputWin "Haskell-File" form Nothing [modal True]


mkHaskellForm = do {
        d <- return (19970612);
        editHaskellDescr (HaskellDescr "Queue" 4 Plain []);
        done
}  





-- TEST DISCONTINUED.
-- Reason - it uses the "newController" function, of which no trace
-- survives even in Einar's code.
-- George Russell, October 1999 

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
import Dynamics

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


main = do {
        print ((read "0") :: Double);
        htk [{- logfile (1::Int) -}];
        setErrorHandler errWin;
        mkCounter;
        mkScrollBox;
        mkLabelBox;
        mkLabel;
        mkMessage;
        mkEntry;
        mkSeparator;
        mkSpace;
        mkClickButton;
        mkCheckButton;
        mkCheckButton2;
        mkRadioButton;
        mkMenuButton;
        mkSpinButton;
        mkScale;
        mkListBox;
        mkScrollBar;
        mkEditor;
        mkComboBox;
        mkMenu;
        mkOptMenu;
        mkCounter;
        mkController;
        mkDialogue;
        mkInputForm;
        mkHaskellForm;
        done
} where errWin e = 
                newErrorWin ("Haskell-Tk error occured: " ++ show e)[modal True]


mkEditor = do
        b <- newVBox []
        mb <- newMenuButton [text "File",parent b,anchor West]  
        mn <- newEditorMenu
        configure mn [parent (mb::MenuButton EditorCmd)]
        ed <- newEditor [size (60,30), bg "white",flexible,value ""]
        sb <- newScrollBox ed [flexible,parent b]
        win <- window b [text "Text Editor"]
        controller' win (receive mn >>>= \f -> f ed)    

type EditorCmd = Editor String -> IO ()

newEditorMenu :: IO (Menu EditorCmd)
newEditorMenu = do
        mn <- newMenu []
        newButton [text "Open...", cmd openFile,  parent mn]
        newButton [text "Save...", cmd saveFile,  parent mn]
        newButton [text "Empty", cmd emptyEditor, parent mn]
        return mn
 where  cmd f = command (\() -> return f)
        openFile tp = withFile (readTextFromFile tp)
        saveFile tp = withFile (writeTextToFile tp)
        emptyEditor :: Editor String -> IO ()
        emptyEditor tp = do {configure tp [value ""]; done}
        withFile cmd =  do
                fnm <- newPromptWin "File Name" "" [modal True]
                incase fnm cmd


mkScrollBox = do
        t <- newEditor [size (20,10), bg "white",value "hello\nworld"]
        sb <- newScrollBox t []
        win <- window sb [text "ScrollBox"]
        controller' win inaction        

mkLabelBox = do
        b <- newVFBox []
        e <- newEntry [value (0::Int), width 5]
        lb <- newLabelBox e [text "Count: ",orient Horizontal,parent b]
        bt <- button [text "Increment", parent b]
        win <- window b [text "LabelBox"]
        controller' win (receive bt >>> updValue e succ)        

 
mkLabel = do
        l <- newLabel [value "hello world",width 30]
        win <- window l [text "Label"]
        controller'  win inaction

mkMessage = do
        m <- newMessage [value [(1::Int)..100],aspect 200]
        win <- window m [text "Message"]
        controller' win inaction


instance GUIValue [Int] where
        cdefault = []


mkEntry = do    
        vb <- newVFBox []
        e1 <- newEntry [value (0::Int),parent vb]
        e2 <- newEntry [value (0::Int),disable,parent vb]
        win <- window vb [text "Entry"]
        controller' win (
                keyPressed e1 "Return" >>> do { 
                        v <- getValue e1; 
                        configure e2 [value (fac v)]
                        }
                )
 where  fac :: Int -> Int
        fac x | x > 1 = x * fac (x-1)
        fac _ = 1


mkSeparator = do        
        vb <- newVFBox [width (cm 4)]
        newLabel [value "End Session?", parent vb]
        newSeparator [parent vb, fill Horizontal]
        b <- button [text "Yes", pad Vertical (cm 0.3), parent vb]
        win <- window vb [text "Separator"]
        controller win (\iact -> 
                receive b >>> do {
                        ans <- newConfirmWin "Do you really want to end the session" 
                                  [modal True];
                        when ans (do {logout; stop iact})
                })


mkSpace = do    
        vb <- newVBox [width (cm 4)]
        newLabel [value "End Session?", flexible, parent vb]
        newSeparator [parent vb, fill Horizontal]
        newSpace (cm 0.3) [parent vb, bg "white"]
        b <- button [text "Yes", parent vb, fill Horizontal]
        win <- window vb [text "Space"]
        controller win (\iact ->
                receive b >>> do {
                        ans <- newConfirmWin "Do you really want to end the session" 
                                  [modal True];
                        when ans (do {logout; stop iact})
                })

mkClickButton = do {    
        vb <- newVFBox [];
        l <- newLabel [value (0::Int), relief Ridge, parent vb];
        b1 <- newButton [text "+",command (\() -> return succ),parent vb];
        b2 <- newButton [text "-",command (\() -> return pred),parent vb];
        win <- window vb [text "Counter"];
        controller' win ((receive b1 +> receive b2) >>>= updValue l)
        }


mkCheckButton = do      
        b <- newVBox []
        e <- newEntry [value "hello world", justify JustCenter,parent b]
        b1 <- checkbutton' b [text "Italic",command updSlant]
        b2 <- checkbutton' b [text "Bold",command updWeight]
        win <- window b [text "CheckButton"]
        controller' win (choose (map receive [b1,b2]) >>>= \newFont -> 
                do {f <- getFont e; font (newFont f) e}) 
 where  checkbutton' b c = newCheckButton (c ++ [parent b, anchor West])
        updSlant :: Toggle -> IO (Font -> XFont)
        updSlant On = return (\f -> xfont{slant = Just Italic})
        updSlant Off = return (\f -> xfont{slant = Just Roman})
        updWeight :: Toggle -> IO (Font -> XFont)
        updWeight Off = return (\f -> xfont{weight = Just NormalWeight})
        updWeight On = return (\f -> xfont{weight = Just Bold}) 
        

mkCheckButton2 = do     
        b <- newVBox []
        b1 <- checkbutton [text "I",parent b]
        b2 <- checkbutton [text "B",parent b]
        selectionState On b1
        selectionState On b2
        win <- window b [text "CheckButton2"]
        controller' win inaction        
        

mkRadioButton = do      
        b <- newVBox []
        e <- newEntry [value "hello", parent b]
        bts <- mapM (radio' b) [ JustLeft, JustCenter, JustRight]
        rg <- radiogroup bts
        win <- window b [text "RadioButton"]
        controller' win (receive rg >>>= flip justify e) 

 where  radio' b j = newRadioButton [
                parent b,text (show j),
                anchor West,
                command (\t -> let x = t::Toggle in return j)
                ]
        

mkMenuButton = do
        mb <- newMenuButton [text "File",indicator On,flexible,relief Raised];
        mn <- newPulldownMenu (mb::MenuButton ()) [tearOff On];
        button [text "Open",command openFile,parent mn]
        button [text "Save",command saveFile,parent mn]
        button [text "Save As...",command saveFileAs,parent mn]
        button [text "Empty",command emptyText,parent mn]
        win <- window mb [text "Menubutton"]
        controller' win (receive mn)
                
 where  openFile () = bell
        saveFile () = bell
        saveFileAs () = bell
        emptyText () = bell     


mkSpinButton = do {
        b <- newHFBox [];
        e <- newEntry [value (0::Int),parent b];
        sb <- newSpinButton [parent b];
        win <- window b [text "SpinButton"];
        interactor (\iact -> 
                destroyed win >>> do {logout; stop iact}
        +>      receive sb >>>= \s -> do {
                        updValue e (case s of {Up -> succ;_ -> pred});
                        done
                        }
        );} 


mkScale = do {
        b <- newHFBox [];
        f <- newFrame [parent b, size (cm 1,cm 4),relief Sunken];
        scs <- mapM (scale' b) ["R","G","B"]; 
        win <- window b [text "Colour Mixer"];
        controller' win (scaled f scs);
        done
} where scale' b col = newScale [interval (0,255), parent b, text col]
        rgb [r,g,b] = (r,g,b::Double)
        scaled :: Frame -> [Scale Double] -> EV ()
        scaled f scs =
                choose (map receive scs) >>> do { 
                        pos <- mapM getValue scs;
                        configure f [bg (rgb pos)];
                        done
                        }


mkListBox = do 
        l <- newListBox [value [Groove,Ridge,Flat,Sunken,Raised]]
        el <- getValue l
        print el
        win <- window l [text "Listbox"]
        controller' win (
                selectionChanged l >>>= mapM (\r -> configure l [relief r]))
 where  selectionChanged :: GUIValue a => ListBox [a] -> EV [a]
        selectionChanged l =            
                mouseButtonPress l 1 >>> do {
                        ix <- getSelection l;
                        case ix of 
                                Just i -> do {
                                        print (i::[Int]);
                                        el <- getValue l;
                                        print el;
                                        return [(el !! (head i))]
                                        } 
                                _ -> do {bell;return []}
                        }




mkScrollBar = do 
        b <- newVFBox []
        e <- newEntry [value "this is a long string",width 10,parent b]
        s <- newScrollBar [parent b, orient Horizontal]
        configure e [scroller Horizontal s]
        win <- window b [text "Scrollbar"]
        controller' win inaction

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


mkMenu = do
        mb0 <- newMenuButton [text "Test",indicator On,flexible,relief Raised];
        mn0 <- newPulldownMenu (mb0::MenuButton String) [tearOff On];
        mb <- newMenuButton [text "Colour", indicator On, parent mn0];
        mn <- newPulldownMenu (mb::MenuButton String) [];
        b1 <- newButton [text "Red",cmd "red",parent mn]
        b2 <- newButton [text "Green",cmd "green",parent mn]
        b3 <- newButton [text "Blue",cmd "blue",parent mn]
        win <- window mb0 [text "Menubutton"]
        mevents <- getReceiveEvent mb0
        controller' win (
                mevents >>>= \c ->
                        foreach [b1,b2,b3] (\b -> do {
                                configure b [bg (c::String)];
                                done
                                })
                )
   where cmd :: String -> Config (Button String)
         cmd str = command (\() -> return str)
                

mkOptMenu = do
        mb <- optmenu ["red","yellow","green"]
        win <- window mb [text "Option Menu"]
        controller' win (receive mb >>>= flip bg mb)
 where  optmenu :: [String] -> IO (MenuButton String)           
        optmenu il = do
                mb <- newMenuButton [ indicator On, flexible,text ((il++[""])!!1)]
                mn <- newPulldownMenu mb [tearOff On]
                mapM (\i -> newButton [text i,cmd mb i,parent mn]) il
                return mb
        cmd :: MenuButton String -> String -> Config (Button String)
        cmd mb i = command (\() -> do {text i mb; return i})
        

mkCounter = do
    cnt <- newCounter [value (5::Int)]
    w <- window cnt [text "Counter"]
    controller' w (receive cnt)


mkController = do
    c <- newController (10::Int)
    sequence [(\_ -> view c n) n | n <- [1..3]]
 where  view c n = do
                cnt <- newCounter []
                w <- window cnt [text ("Counter"++show n)]
                registerView c (receive cnt,setValue cnt,destroyed w)
        setValue cnt v = value v cnt >> done


mkDialogue = do
        newErrorWin "Illegal integer value" [modal True]
        done

data Gender = Male | Female deriving (Eq,Ord,Read,Show,Enum)

instance GUIValue Gender where
        cdefault = Male

data Record = Record {
        no ::Int, 
        nm :: String, 
        gender :: Gender,
        addr :: Address,
        cv::String
        } deriving (Eq,Read,Show)

instance GUIValue Record where
        cdefault = Record cdefault cdefault cdefault cdefault cdefault

data Address = Address {
        street :: String, 
        city :: String, 
        zipcode :: Int,
        country :: String
        } deriving (Eq,Read,Show)

instance GUIValue Address where
        cdefault = Address "" "" 0 ""

mkAddressForm = do {
        form <- newInputForm [width (cm 20), flexible];
        value cdefault form;
        newEntryField [text "Street:",
                selector street,
                replacor (\r street -> r{street}),
                bg "white",
                parent form, 
                width 40
                ];
        newEntryField [text "City:",
                selector street,
                replacor (\r city -> r{city}),
                bg "white",
                parent form, 
                width 40
                ];
        newEntryField [text "Zipcode:",
                selector zipcode,
                bg "white",
                replacor (\r zipcode -> r{zipcode}),
                parent form, 
                width 60
                ];
        newEntryField [text "Country:",
                selector country,
                replacor (\r country -> r{country}),
                bg "white",
                parent form, 
                width 40
                ];
        return form
} 


mkInputForm = do {
        form <- newInputForm [width (cm 20), bg "red", fg "blue", flexible];
        value (Record {no = 1, nm = "hello", gender = Male,cv = "", addr = cdefault}) form;
        newEntryField [text "Name:",
                selector nm,
                replacor (\r -> \nm -> r{nm}),
                bg "white",
                parent form, 
                width 60
                ];
        newEntryField [text "Age:",
                selector no,
                bg "white",
                replacor (\r -> \no -> r{no}),
                parent form, 
                width 60
                ];
        newEnumField [Male,Female] [text "Gender:",
                selector gender,
                bg "white",
                replacor (\r -> \gender -> r{gender}),
                parent form, 
                width 60
                ];
        newTextField [text "Curriculum Vitae:",
                selector cv,
                replacor (\r cv -> r{cv}),
                parent form, 
                width 60,
                height 6
                ];

        cf <- mkAddressForm;
        value cdefault cf;

        newRecordField cf [
                text "Address:",
                selector addr,
                replacor (\r addr -> r{addr}),
                parent form, 
                width 40
                ];

        ans <- newInputWin "Enter Input" form Nothing [];
        case ans of 
                Nothing -> mkInputForm
                (Just val) -> done
} 


data HaskellDescr =  HaskellDescr {fName :: String, fVersion :: Int, fKind :: HaskellKind, fText :: [String]}

data HaskellKind = Plain | Literal deriving (Eq,Ord,Show,Read)

instance GUIValue HaskellKind where
        cdefault = Plain

editHaskellDescr :: HaskellDescr -> IO (Maybe HaskellDescr)
editHaskellDescr zd = do {
        form <- newInputForm [width (cm 20), flexible];
        value zd form;
        newEntryField [text "Name:",
                selector fName,
                replacor (\r fName -> r{fName}),
                parent form
                ];
        newEntryField [text "Version Id:",
                selector fVersion,
                replacor (\r fVersion -> r{fVersion}),
                parent form
                ];
        newTextField [text "Description:",
                selector (fText),
                replacor (\r t -> r{fText = t}),
                parent form, 
                width 20,
                height 10
                ];
        newEnumField [Plain,Literal] [
                text "Haskell-Kind:", 
                selector fKind,
                replacor (\r fKind -> r{fKind}), 
                parent form
                ];
        newInputWin "Haskell-File" form Nothing [modal True];   
} 


mkHaskellForm = do {
        d <- return (19970612);
        editHaskellDescr (HaskellDescr "Queue" 4 Plain []);
        done
}  


-- ------------------------------------------------------------------------

data Counter a = Counter Box (Entry a) [Button (a -> a)]

newCounter :: [Config (Counter Int)] -> IO (Counter Int)
newCounter cs = do
    b <- newVFBox []
    e <- newEntry [value (0::Int), disable, width 8, bg "white",parent b]
--  l <- newLabelBox e [text "Count", parent b]
    b1 <- button [text "+", parent b]
    b2 <- button [text "-", parent b]
    return (Counter b e [map (f succ) b1,map (f pred) b2])
 where f g = \_ -> g

instance GUIObject (Counter a) where
        toGUIObject (Counter b _ _) = toGUIObject b
instance Widget (Counter a)
instance ChildWidget (Counter a)
instance HasFont (Counter a)
instance HasSize (Counter a)
instance HasColour (Counter a)
instance Synchronized (Counter a) where
        synchronize (Counter b _ _) = synchronize b

instance GUIValue a => HasReceiveEV Counter a where
        receive (Counter _ e bts) = 
                choose (map receive bts) >>>= updValue e

instance GUIValue a => HasValue Counter a where
        value v c@(Counter _ ent _) = value v ent >> return c
        getValue c@(Counter _ ent _) = getValue ent




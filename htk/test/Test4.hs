-- TEST DISCONTINUED.
-- Reason - it uses the "newController" function, of which no trace
-- survives even in Einar's code.
-- George Russell, October 1999 

module Main (
        main 
        ) 
where
import Date

import HTk
import SIM
import Dynamics

import Counter
import Prompt
import IconBar
import DialogWin

import Font

import Mouse
import Screen
import Printer
import Index
import Selection
import ICursor
import Slider
import Icon
import Keyboard
import Bell

import Frame
import Box
import Label
import Image
import BitMap
import Message
import Entry
import ListBox
import Scale
import ScrollBar
import Button
import RadioButton
import CheckButton
import MenuButton
import Separator
import Menu
import PulldownMenu
import RadioGroup


import Canvas
import CanvasItem
import CanvasTag
import Arc
import Line
import Oval
import Polygon
import Rectangle
import EmbeddedCanvasWin
import TextItem
import BitMapItem
import ImageItem

import Editor
import TextTag
import EmbeddedTextWin

import InputForm
import InputWin
import PromptWin
import LogWin

import SpinButton
import OptionMenu
import ScrollBox
import LabelBox
import ComboBox

import GUIWish

import TestUtil (logout)
import Debug(debug)

main :: IO ()
main = do {
        htk [{- logfile (1::Int), -} text "HTK-TEST"];
        testcallback;
        testmvc1;
        testnewListBox;
        foreach  [1..10]
                (\i ->  newEditor [bg "white", value (show i)] >>= \ t -> 
                        window t [text ("Win" ++ show i)]        >>= \ win ->
                        controller' win inaction >>
                        done
                );
        testmvc;
        testwindow;
        testinputform;
        testTextWidgets;
        helloWorld;
        testnewEntry;
        testiconbar;
        testinputcheck;
        testinputwin;
        testradiobox;
        testpromptwin;
        testTornOffMenu;
        testSpinButton;
        testoptionmenu;
        scrollablewidget;
        testnewLabelBox;
        testnewComboBox;
        newLogWin [text "Log Window"];
        testzinputform;
        testnewScrollBox;
        testicursor;
        cursormenu;
        done
}


testwindow = do {
        b <- newHFBox [];
        win <- newWindow b [text "Test Window"];
        tp <- newEditor [bg "white", value "hello\nworld"];
        lb <- newScrollBox tp [parent b];
        controller' win inaction;
        renderWindow win        
        }

testcallback = do {
        cbm <- newGUIEventLoop;
        enterEventLoop cbm;
        cnts <- sequence [(\_ -> mkCounter) n | n <- [1..10]];
        foreach cnts (\cnt -> cbm # registerEH cnt (newCounterEvents cbm cnt))
} where newCounterEvents cbm cnt = 
                receive cnt >>> done
            +>  destroyed cnt >>> cbm # deregisterEH cnt
        mkCounter = do {
                cnt <- newCounter [];
                win <- window cnt [text "Counter"];
                return cnt
                }


testmvc1 = do {
        c <- newController 10;
        sequence [(\_ -> controlCounter c n) n | n <- [1..5]]
}

controlCounter c n = do {
        cnt <- newCounter [flexible];
        win <- window cnt [text ("Counter" ++ show n)];
        registerView c (receive cnt,\v -> do{value v cnt; done},destroyed win);
}

testmvc = do {
        c <- newController 10;
        copyMVC c
}

copyMVC c = do {
        b <- newVBox [flexible];
        cnt <- newCounter [flexible, parent b];
        bt <- button [text "Copy View",parent b,flexible];
        bt2 <- button [text "Logout",parent b,flexible];
        registerView c (receive cnt,\v -> do {try (value v cnt);done},destroyed cnt);
        win <- window b [text "MVC"];
        interactor (\iact -> 
{-              receive cnt >>> become (receive cnt >>> done)
           +> -}        destroyed win >>> done
           +>           receive bt >>> copyMVC c
           +>           receive bt2 >>> do {logout; stop iact} 
        );
        done
}



testnewListBox =
        newListBox [value ["red","green","blue","grey"]]        >>= \ lb ->
        openRootWin lb []                       >>= \ win ->
        done


helloWorld =
        newLabel [value "Hello World", 
                font (Helvetica,Bold,120 :: Int)]               >>= \ lbl ->
        window lbl [text "Label"]                               >>= \ win ->
        controller' win inaction


collectWin =
        button [text "Terminate"]                               >>= \ butt ->
        window butt [text "Collector"]                          >>= \ win ->
        sync ( receive butt >>> done)                           >>
        ringBell (Just win)                                     >> 
        delay (secs 5)                                          >>
        putStr "logging out\n"                                  >>
        logout


testnewEntry =
        newVFBox []                                             >>= \vb ->
        newLabel [value "Text Input", parent vb]                        >>
        newEntry [value "",parent vb]                           >>= \e1 ->
        newEntry [value "",parent vb]                           >>= \e2 ->
        newLabel [value "Integer Input", parent vb]             >>
        newEntry [value (0::Int),parent vb]                     >>= \e3 ->
        newEntry [value (0::Int),parent vb]                     >>= \e4 ->
        window vb [text "Entry IO"]                             >>= \win ->
        controller' win (
                   keyPressed e1 "Return" >>>   
                        (getValue e1 >>= \t -> 
                                configure e2 [value t] >> done)
                +> keyPressed e3 "Return" >>>   
                        (getValue e3 >>= \t -> configure e4 [value ((1::Int) + t)] >> done )
                )


testiconbar = do {
        ib <- newIconBar [orient Horizontal];
        newButton [parent ib,
                command (\() -> return "i1"), 
                bitmap "icons/trash.bm"
                ];
        newButton [parent ib,
                command (\() -> return "i2"),
                bitmap "icons/bullseye.bm"
                ];
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
        foreach bts (\eib -> configure eib [width (cm 4), height (cm 4)]);
        win <- window (ib :: IconBar String) [text "IconBar"];
        controller' win (receive ib >>> bell )
        }



testinputcheck = do {
        b <- newVFBox [];
        pr <- newPrompt [value Groove, text "   Relief:  ",parent b];
        bt <- newButton [
                command (\() -> try (getValue pr)),
                text "Check Relief",
                parent b];
        win <- window b [text "Input Checking"];        
        controller' win (checkinput bt);
        done
        } where checkinput bt =
                        receive bt >>>= \ ans -> do {
                                case ans of
                                   Left e -> newErrorWin "Illegal Input" [modal True]
                                   Right _ -> done
                        }


testinputwin = do {
        newPromptWin  "Enter Integer" ((1::Int)) [modal True];
        done
        }


testradiobox = do {
        rb <- newVFBox [];
        b1 <- newRadioButton [reaction (return "green"),text "Green",parent rb];
        b2 <- newRadioButton [reaction (return "red"),text "Red",parent rb];
        b3 <- newRadioButton [reaction (return "blue"),text "Blue",parent rb];
        rg <- radiogroup [b1,b2,b3];
        win <- window rb [text "Radio Group"];
        controller' win (receive rg >>>= (\ c -> foreach [b1,b2,b3] (fg c) >> done));
} where reaction f = command (\t -> let x = t::Toggle in f) 

testinputform = do {
        form <- newInputForm [width (cm 20), flexible];
        value (Record {no = 1, nm = "hello", descr = []}) form;
        newEnumField [1..10] [
                text "Id:", 
                selector no,
                replacor (\r -> \no -> r{no=no}), 
                parent form,
                value (2::Int)
                ];
        newEntryField [text "Name:",
                selector nm,
                replacor (\r -> \nm -> r{nm=nm}),
                parent form, 
                width 60
                ];
        newTextField [text "Description:",
                selector (descr),
                replacor (\r -> \t -> r{descr = t}),
                parent form, 
                width 60,
                height 4
                ];
        ans <- newInputWin "Enter Input" form Nothing [];
        case ans of 
                Nothing -> testinputform
                (Just val) -> done
} 

data ZDescr =  ZDescr {fName :: String, fDate :: Int, fKind :: ZKind, fText :: [String]}

data ZKind = Email | Latex | BoxText deriving (Eq,Ord,Show,Read)

instance GUIValue ZKind where
        cdefault = Email

editZDescr :: ZDescr -> IO (Maybe ZDescr)
editZDescr zd = do {
        form <- newInputForm [width (cm 20), flexible];
        value zd form;
        newEntryField [text "Name:",
                selector fName,
                replacor (\r fName -> r{fName=fName}),
                parent form
                ];
        newEnumField [Email,Latex,BoxText] [
                text "Kind:", 
                selector fKind,
                replacor (\r fKind -> r{fKind=fKind}), 
                parent form
                ];
        newEntryField [text "Date:",
                selector fDate,
                replacor (\r fDate -> r{fDate=fDate}),
                parent form
                ];
        newTextField [text "Description:",
                selector (fText),
                replacor (\r t -> r{fText = t}),
                parent form, 
                width 60,
                height 4
                ];
        newInputWin "Z Object" form Nothing [modal True];
        
} 


testzinputform = do {
        d <- return (19970612);
        editZDescr (ZDescr "Set" d Latex []);
        done
}  

data Record = Record {no ::Int, nm :: String, descr :: [String]}


testpromptwin = do {
        ans <- newPromptWin "Enter Integer" (cdefault::Int) [];
        case ans of 
                Nothing -> testpromptwin
                (Just val) -> done      
}


testTextWidgets = do {
        b <- newVBox [];
                l1 <- newLabel [value (1 ::Int),parent b];
                l2 <- newLabel [value "hello",parent b];
                l3 <- newLabel [value Disabled,parent b];
        window b [text "Test Label"];
        v1 <- getValue l1;
        configure l1 [value v1];
        v1 <- getValue l2;
        configure l2 [value v1];
        v1 <- getValue l3;
        configure l3 [value v1];
        done;

        b <- newVBox [];
                l1 <- newMessage [value (1 ::Int),parent b];
                l2 <- newMessage [value "hello",parent b];
                l3 <- newMessage [value Disabled,parent b];
                l4 <- newMessage [value "hello\nthere",parent b];
        window b [text "Test Message"];
        v1 <- getValue l1;
        configure l1 [value v1];
        v1 <- getValue l2;
        configure l2 [value v1];
        v1 <- getValue l3;
        configure l3 [value v1];
        v1 <- getValue l4;
        configure l4 [value v1];
        done;

        b <- newVBox [];
                l1 <- newEntry [value (1 ::Int),parent b];
                l2 <- newEntry [value "hello",parent b];
                l3 <- newEntry [value Disabled,parent b];
        window b [text "Test Entry"];
        v1 <- getValue l1;
        configure l1 [value v1];
        v1 <- getValue l2;
        configure l2 [value v1];
        v1 <- getValue l3;
        configure l3 [value v1];
        done
}

testTornOffMenu :: IO ()
testTornOffMenu = do {
        bt <- newMenuButton [text "hello"];
        mn <- newPulldownMenu (bt::MenuButton ()) [];

        el <- newGUIEventLoop;

        b1 <- button [parent mn,text "Delete"];
        b2 <- button [parent mn,text "Insert"];

        el # registerEH b1 (receive b1 >>> bell);
        el # registerEH b2 (receive b2 >>> bell);

        win <- window bt [text "Test Menu"];
        el # registerEH win (destroyed win >>> shutdown);
        enterEventLoop el;
        done
}


testSpinButton :: IO ()
testSpinButton = do {
        b <- newHFBox [];
        e <- newEntry [value (0::Int),parent b];
        sb <- newSpinButton [parent b];
        win <- window b [text "Count"];
        interactor (\iact ->
                spinned sb e >>> done
           +>   destroyed win >>> do {logout;stop iact}
        );
        done
} 


spinned :: SpinButton Spin -> Entry Int -> EV Int
spinned sb e = 
        receive sb >>>= \s -> 
                if s == Up then updValue e succ else updValue e pred


testoptionmenu :: IO ()
testoptionmenu = do {
        b <- newVFBox [];
        newOptionMenu [1::Int,2,3,4] [fg "blue", bg "red",value (2::Int),parent b];
        newOptionMenu ["a","b","c","d"] [parent b,
                        fg "blue", bg "red",value "a",
                        activeBackground "red", activeForeground "blue"];
        win <- window b [text "Test OptionMenu"];
        controller' win inaction;
        done
}


scrollablewidget :: IO ()
scrollablewidget = do {
        t <- newEditor [bg "white", value "test"];
        st <- newScrollBox t [fill Both];
        win <- window st [text "Test Scroller"];
        controller' win inaction;
        done
}

testnewLabelBox :: IO ()
testnewLabelBox = do {
        b <- newHFBox [];
        e <- newEntry [value (0::Int),parent b];
        sb <- newSpinButton [bg "red", fg "blue",parent b];
        lb <- newLabelBox b [orient Vertical, fill Both, text "Count"];
        win <- window lb [text "Test LabelBox"];
        interactor (\iact ->
                spinned sb e >>> done
           +>   destroyed win >>> do {logout; stop iact}
        );
        done
}

testnewComboBox = do {
        cb <- newComboBox [bg "blue"];
        value ["first", "second","third"] (fListBox cb);
        win <- window (cb :: ComboBox String) [text "Test ComboBox"];
        controller' win (receive (fListBox cb));
        done    
}


testnewScrollBox = do {
        e <- newEntry [value "this is a string, to long to be completely displayed"];
        lb <- newScrollBox e [];
        openRootWin lb [text "ScrollBox"]
        }


testicursor = do {
        ent <- newEntry [fg "red", value "Hello World"];
        win <- window ent [text "Entry"];
        configure (ICursor ent) [fg "blue", width (cm 0.1)];
        controller' win inaction;
        done
}


cursormenu = do {
        cnt <- newCounter [];
        win1 <- window cnt [text "Counter"];
        mn <- newMenu [];
        b1 <- newButton [text "+", 
                command (\() -> return (+ (1::Int))), 
                parent mn
                ];
        b2 <- newButton [text "-", 
                command (\() -> return (\x -> x-(1::Int))), 
                parent mn
                ];
        b3 <- newButton [text "0", 
                command (\() -> return (\v -> (0::Int))),
                parent mn
                ];
        win2 <- window (mn::Menu(Int -> Int)) [];
        post mn (cm 15, cm 15); 
        controller' win2 (
                receive b1 >>>= (\f -> updValue cnt f >> done)
            +>  receive b2 >>>= (\f -> updValue cnt f >> done)
            +>  receive b3 >>>= (\f -> updValue cnt f >> done)
            +>  receive cnt >>> done
        );
        done
}


openRootWin w confs = do
        win <- window w confs
        interactor (\iact -> destroyed win >>> do {logout;stop iact})
        return win



{- #########################################################################

MODULE        : TestSystem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   :  


   ######################################################################### -}

module Main (
        main 
        ) 
where

import HTk
import Concurrency

import Counter
import Prompt
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

import Posix(getEnvVar)

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

import Util
import IO(stdout)
import Debug(debug)

main :: IO ()
main =
   do
      htk []                   
      setLogFile (Just stdout) 
      setErrorHandler errWin   
      testLabel                
      testMessage              
      testEntry                
      testScale                
      testClickButton          
      testCheckButton          
      testRadioButton          
      testListBox              
      testScrollBar            
      testMenu                 
      testPopupMenu            
      testEditor               
      testCanvas               
      block
   where 
      disp = "hydra:0.0"
      errWin e = forkIOnull (
         newErrorWin ("Haskell-Tk error occured: " ++ show e)[]
         )



-- --------------------------------------------------------------------------
-- LABEL
-- --------------------------------------------------------------------------           
testLabel =
        newVBox []                                                      >>= \ box ->
        newLabel [value "Label1", parent box]                   >>= \ lb1 ->
        newLabel [wbbmap flagup, parent box]                    >>= \ lb2 ->
        newLabel [bitmap hourglass, parent box]                 >>= \ lb3 ->
        newImage [filename "~ewk/www/ewk.gif"]                  >>= \ img ->
        newLabel [photo img, parent box]                                >>= \ lb4 ->
        window box [text "Label Test"]                          >>= \ win ->
        controller win (const inaction)         >>
        newLabel [value "hello there",
                flexible,
                parent box,
                setBaseWidget,
                setWidgetColour,
                setFont,
                setJustify,
                setSize
                ]                                               >>= \lb5 ->
        newLabel [value [1..10],parent box]                     >>= \lb6 ->
        runLabelTest (lb1 :: Label String)                      >>
        runTextLabelTest lb1                                    >>
        runLabelTest  (lb2 :: Label BitMap)                     >>
        getBitMap lb2                                           >>
        runLabelTest (lb3 :: Label BitMap)                      >>
        getBitMap lb3                                           >>
        runLabelTest lb4                                        >>
        getPhoto (lb4 :: Label Image)                           >>
        runLabelTest (lb5 :: Label String)                      >>
        runLabelTest (lb6 :: Label [Int])                       >>
        done 
        where runLabelTest :: Label a -> IO ()
              runLabelTest cb =
                testBaseWidget cb                               >>
                testHasColour cb                                        >>
                testHighlightedWidget cb                        >>
                testHasColumns cb                               >>
                testHasAnchor cb                                >>
                done
              runTextLabelTest :: Label String -> IO ()
              runTextLabelTest cb =
                testHasJustify cb                               >>
                testHasUnderline cb                             >>
                testHasFont cb                                  >>
                testHasValue cb                                 >>
                testTextStateWidget cb                          >>
                ((getValue cb) :: IO String)                    >>= \ msg ->
                value msg cb                                    >>      
                done
              flagup = "trash.bm"


-- --------------------------------------------------------------------------
-- Message
-- --------------------------------------------------------------------------           
testMessage =
        newHFBox []                                             >>= \ box ->
        newMessage [value "Test Message",parent box]            >>= \ cb ->
        window box [text "Test Message"]                        >>= \ win ->
        controller win (const inaction)                 >>
        newMessage [value "hello there",
                aspect 4,
                flexible,
                parent box,
                setBaseWidget,
                setWidgetColour,
                setFont,
                setJustify,
                setSize
                ]                                               >>= \msg2 ->
        newMessage [value [1..10],parent box]                   >>= \msg3 ->
        runTextMessageTest cb                                   >>
        runTextMessageTest msg2                                 >>
        runMessageTest (msg3 :: Message [Int])                  >>
        done
        
        where runMessageTest :: GUIValue a => Message a -> IO ()
              runMessageTest cb =
                fetchC cb getAspect                             >>
                testBaseWidget cb                               >>
                testHasColour cb                                >>
                testHighlightedWidget cb                        >>
                testHasWidth cb                                 >>
                testHasFont cb                                  >>
                testHasValue cb                                 >>
                testHasAnchor cb                                >>
                testHasJustify cb                               >>
                getValue cb                                     >>= \ msg ->
                value msg cb                                    >>
                done
              runTextMessageTest :: Message String -> IO ()
              runTextMessageTest cb =
                runMessageTest cb                               >>
                testTextStateWidget cb                          >>
                done


-- --------------------------------------------------------------------------
-- Entry
-- --------------------------------------------------------------------------           

testEntry =
        newVBox []                                                      >>= \box ->
        newEntry [value "hello", showText '*',parent box]               >>= \ ent1 ->
        newEntry [value "hello",parent box]                     >>= \ ent2 ->
        newEntry [value (10::Int),parent box]                   >>= \ ent3 ->
        newEntry [value (""::String),parent box]                        >>= \ ent4 ->
        window box [text "Test Entry"]                          >>= \ win ->
        controller win (const inaction)                         >>
        newEntry [value "hello there",
                showText '*',
                flexible,
                parent box,
                setBaseWidget,
                setWidgetColour,
                setFont,
                setEnable,
                setJustify,
                setSize,
                setICursor,
                setSelection
                ]                                               >>= \ent5 ->
        newEntry [value [1..10],parent box]                     >>= \ent6 -> 
        runTestTextEntry (ent1 :: Entry String)                 >>
        runTestTextEntry (ent2  :: Entry String)                >>
        runTestEntry (ent3 :: Entry Int)                        >>
        runTestEntry (ent4 :: Entry String)                     >>
        runTestEntry (ent5 :: Entry String)                     >>
        runTestEntry (ent6 :: Entry [Int])                      >>
        done
        where runTestEntry :: GUIValue a => Entry a -> IO ()
              runTestEntry cb =
--              fetchC cb getShowText                           >>
                testBaseWidget cb                               >>
                testHasColour cb                                >>
                testHighlightedWidget cb                        >>
                testHasColumns cb                               >>
                testHasFont cb                                  >>
                testHasEnable cb                                >>
                testHasJustify cb                               >>
                testSelection cb                                >>
                testHasICursor cb                               >>
                getValue cb                                     >>= \ msg ->
                value msg cb                                    >>
                testHasSelection cb (0 :: Int) (5::Int)         >>
                testHasInsertionCursor cb (0 :: Int)            >>
                testHasInsertionCursor cb (1 :: Int)            >>
                testHasIndex cb EndOfText                       >>
                testHasIndex cb (1::Int)                        >>
                testHasIndex cb (XCoord (1::Distance))          >>
                testHasIndex cb (ICursor cb)                    >>
                testHasIndex cb (Selection cb,First)            >>
                testHasIndex cb (Selection cb,Last)             >>
                done
              runTestTextEntry :: Entry String -> IO ()
              runTestTextEntry cb =
                runTestEntry cb                                 >>
                testHasFont cb                                  >>
                testHasJustify cb                               >>
                testTextStateWidget cb                          >>
                done



testHasIndex :: (HasIndex (Entry a) i Int,
        HasIndex (Entry a) i BaseIndex) => Entry a -> i -> IO ()
testHasIndex cb i = do {
        ans <- try(getBaseIndex cb i);
        case ans of
                Left e -> done
                Right v -> do {
                        print (v::Int);
                        testEntrySelection cb i
                        }
        }

testEntrySelection :: HasIndex (Entry a) i BaseIndex => Entry a -> i -> IO ()
testEntrySelection cb i = do {
        selection i cb;
        isSelected cb i;
        clearSelection cb;
}

-- --------------------------------------------------------------------------
-- ClickButton
-- --------------------------------------------------------------------------           
testClickButton =
        newEventLoop                                            >>= \ el ->
        newVFBox []                                             >>= \ box ->
        button [text "Test Button1", parent box]                >>= \ cb1 ->
        button [text "Test Button2", parent box,
                action el (triggered @>> bell)]                 >>= \ cb2 ->
        button [text "Ring Bell", 
                accelerator "Ctrl-D", parent box,
                action el (triggered @>> bell) ]        >>= \ cb3 ->
        button [text "Flash B1", 
                action el (triggered @>> flash cb1), parent box]        >>
        button [text "Invoke B1", parent box,
                action el (triggered @>> invoke cb1)]           >>
        button [text "Flash B2", 
                action el (triggered @>> flash cb2), parent box]        >>
        button [text "Invoke B2", parent box,
                action el (triggered @>> invoke cb2)]           >>
        window box [text "TestButton",
                action el (destroyed @>> destroy el)]           >>
        enterEventLoop el                                       >>
        testHasAccelerator cb1                                  >>
        testHasAccelerator cb2                                  >>
        testHasAccelerator cb3                                  >>
        testButtonWidget cb1                                    >>
        testButtonWidget cb2                                    >>
        testButtonWidget cb3                                    >>
        button [text "Invoke B2", 
                parent box,
                action el (triggered @>> invoke cb2),
                setButtonWidget
                ]                                               >>= \bt4 ->
        testHasAccelerator bt4                                  >>
        testButtonWidget bt4                                    >>
        done


-- --------------------------------------------------------------------------
-- CheckButton
-- --------------------------------------------------------------------------           
testCheckButton =
        newEventLoop                                            >>= \el ->
        newVFBox []                                             >>= \ box ->
        newCheckButton [text "Check Button", parent box,
                action el (triggered @>> bell) ]                >>= \ cb ->
        newButton [text "Flash CB", parent box,
                action el (triggered @>> flash cb)]             >>
        newButton [text "Invoke CB", parent box,
                action el (triggered @>> invoke cb)]            >>
        newButton [text "Select CB", parent box,
                action el (triggered @>> (selectionState On cb >> done))]       >>
        newButton [text "Deselect CB", parent box,
                action el (triggered @>> (selectionState Off cb >> done))]      >>
        newButton [text "Toggle CB", parent box,
                action el (triggered @>> toggleButton cb)]      >>
        window box [text "Test Check Button",
                action el (destroyed @>> destroy el)]           >>= \ win ->
        enterEventLoop el                                       >>
        newCheckButton [text "CheckButton 2", 
                parent box,
                action el (triggered @>> bell),
                setButtonWidget
                ]                                               >>= \bt4 ->
        testSelectButtonWidget cb                               >>
        testSelectButtonWidget bt4                              >>
        done


-- --------------------------------------------------------------------------
-- CheckButton
-- --------------------------------------------------------------------------           
testRadioButton =
        newEventLoop                                            >>= \ el ->
        newVFBox []                                             >>= \ box ->
        newRadioButton [text "Radio Button", parent box,
                action el (triggered @>> bell)]                 >>= \ cb ->
        newButton [text "Flash RB", parent box, 
                action el (triggered @>> flash cb)]             >>
        newButton [text "Invoke RB", parent box,
                action el (triggered @>> invoke cb)]            >>
        newButton [text "Select RB", parent box,
                action el (triggered @>> (selectionState On cb >> done))]       >>
        newButton [text "Deselect RB", parent box,
                action el (triggered @>> (selectionState Off cb >> done))]      >>
        window box [text "Test Radio Button",
                action el (destroyed @>> destroy el)]           >>
        enterEventLoop el                                       >>
        newRadioButton [text "RadioButton 2", 
                parent box,
                action el (triggered @>> bell),
                setButtonWidget
                ]                                               >>= \bt4 ->
        testSelectButtonWidget cb                               >>
        testSelectButtonWidget bt4                              >>
        done


-- --------------------------------------------------------------------------
-- CANVAS
-- --------------------------------------------------------------------------           
testCanvas =
        newHFBox []                                             >>= \ box -> 
        newCanvas [width (cm 15),height (cm 15),background white,
                flexible]                                       >>= \ cv ->
        runCanvasTest cv                                                >>
        testCanvasItem cv                                       >>
        window cv [text "TestCanvas"]                           >>= \ win ->
        controller win (const inaction)                         >>= \ master ->
        newCanvas [width (cm 3),
                flexible,
                parent box,
                height (cm 3),
                background "red",
                closeEnough 5,
                confine True,
                scrollRegion ((1,1),(1,1),(1,1),(1,1)),
                scrollIncrement Horizontal 1,
                scrollIncrement Vertical 1              
                ]                                               >>= \ cv2 ->
        runCanvasTest cv2                                       >>
        testCanvasItem cv2                                      >>
        newRectangle [parent cv, size (100,100)]                        >>= \ cir ->
        testRectangleItem cir                                   >>
        newRectangle [parent cv,size (90,90),filling blue,outline red] >>= \ cir ->
        testRectangleItem cir                                   >>
        newOval [parent cv, pos (100,100),size (200,200)]               >>= \ cir ->
        testOvalItem cir                                        >>
        newOval [parent cv, coord [(150,400),(200,500)],
              filling blue, outline red]                        >>= \ ov ->
        testOvalItem ov                                         >>
        newPolygon [parent cv, coord [(50,50),(100,100),(200,200)]]     >>= \ cir ->
        testPolygonItem cir                                     >>
        newArc [parent cv, geo (50,50,50,50),extent 90]         >>= \ cir ->
        testArcItem cir                                         >>
        newLine [parent cv, coord [(50,50),(0,100),(300,300)]]  >>= \ cir ->
        newCounter []                                           >>= \ cnt ->
        newEmbeddedCanvasWin cnt [pos (200,300),parent cv]      >>= \ ewin ->
        configure ewin [anchor West]                            >>
        controller cnt (\_ -> triggered cnt)                    >>= \ iact ->
        testLineItem cir                                        >>
        newTextItem [parent cv, pos (200,200),value "howdy"]    >>= \ cir ->
        moveItem cir 0 0                                        >>
        testTextItem cir                                        >>
        newBitMapItem [parent cv, pos (100,200)]                        >>= \ cir ->
        testBitMapItem cir                                      >>
        newBitMapItem [parent cv, 
                position (100,200),
                wbbmap "trash.bm" {-,
                anchor Center -}
                ]                                               >>= \ cir ->
        testBitMapItem cir                                      >>
        newImage [filename "~ewk/www/ewk.gif"]                  >>= \ img ->
        newImageItem [parent cv, 
                position (200,100),
                photo img {-,
                anchor Center -}
                ]                                               >>= \ cir ->
        testImageItem cir                                       >>

        newCanvasTag allItems [parent cv]                               >>= \ tg ->
        moveItem tg 10 10                                       >>

        newCanvasTag (withTag cir) [parent cv]                  >>= \ tg2 ->
        moveItem tg2 100 100                                    >>

        controller cv  (\_ -> mouseMotion cv >>>= \ p -> do {
                configure ov [position p];
                done 
                })                                              >>
        testPrintable cv "newCanvas.ps"                         >>
        done


runCanvasTest :: Canvas -> IO ()
runCanvasTest cv = do {
--      testWidget cv;
        testHasBorder cv;
        testHasColour cv;
        testHasEnable cv;
--      testHasSize;
        try(testPrintable cv "canvas.ps");
        fetchC cv getCloseEnough;
        fetchC cv getConfine;
--      fetchC cv getScrollRegion;
        fetchC cv (getScrollIncrement Horizontal);
        fetchC cv (getScrollIncrement Vertical);
        done
}

-- --------------------------------------------------------------------------
-- MENU
-- --------------------------------------------------------------------------           
testMenu :: IO ()
testMenu =
        newGUIEventLoop                                         >>= \ el ->
        newMenuButton [text "Test Menu"]                                >>= \ but ->
        newPulldownMenu (but :: (MenuButton ())) []             >>= \ mn ->
        newSeparator [parent (mn::Menu ())]                                     >>= \ sp ->
        newRadioButton' [text "Yes",parent mn]                  >>= \ rb ->
        newCheckButton' [text "Auto Save", parent mn]           >>= \ cb ->     
        button [text "Show", parent mn]                         >>= \ b1 ->
        newMenuButton [text "SubMenu",parent mn]                        >>= \ but2 ->
        newPulldownMenu (but2::MenuButton ()) []                        >>= \ mn2 ->
        button [text "Show", parent (mn2::Menu ())]             >>= \ b2 ->
        window but [text "Test Menu"]                           >>= \ win ->
        configure b1 [fg red]                                   >>
        el # registerEH win (destroyed win)                     >>
        el # registerEH b1 (triggered b1 >>> bell)              >>
        el # registerEH b2 (triggered b2 >>> bell)              >>
        enterEventLoop  el                                      >>
        testMenuButtonWidget but                                >>
        testMenuWidget mn                                       >>      
        testButtonWidget b1                                     >>
        testSelectButtonWidget cb                               >>
        testSelectButtonWidget rb                               >>
        testSeparatorWidget (sp)                                >>
        testMenuWidget mn2                                      >>      
        testMenuButtonWidget but2                               >>
        done

newRadioButton' :: [Config (RadioButton ())] -> IO (RadioButton ())
newRadioButton' cnfs = do {
        b <- newRadioButton [];
        b' <- mapTrigger (\_ -> return ()) b;
        configure b' cnfs
        }

newCheckButton' :: [Config (CheckButton ())] -> IO (CheckButton ())
newCheckButton' cnfs = do {
        b <- newCheckButton [];
        b' <- mapTrigger (\_ -> return ()) b;
        configure b' cnfs
        }



-- --------------------------------------------------------------------------
-- MENU
-- --------------------------------------------------------------------------           
testPopupMenu :: IO ()
testPopupMenu =
        newMenu []                                                      >>= \ mn ->
        newSeparator [parent (mn::Menu ())]                     >>= \ sp ->
        newRadioButton' [text "Yes",parent mn]                  >>= \ rb ->
        newCheckButton' [text "Auto Save", parent mn]           >>= \ cb ->     
        button [text "Show", parent mn]                         >>= \ b ->
        window mn [text "Test Toplevel Menu"]                   >>= \ win ->
        controller win (\_ -> inaction)                         >>
        post mn (cm 5,cm 10)                                    >>
        newCanvas [bg white,flexible]                           >>= \ cv ->
        window cv [text "Popup Menu"]                           >>= \ win ->
        controller win (\_ ->
           mouseButtonPress cv 3 >>>= (\ (mx,my) -> 
                  getPosition win       >>= \ (wx,wy) ->
                  post mn (mx + wx, my + wy) >>
                  done)
        )                                                       >>
        testMenuWidget mn                                       >>      
        testButtonWidget b                                      >>
        testSelectButtonWidget cb                               >>
        testSelectButtonWidget rb                               >>
        testSeparatorWidget (sp :: Separator)                   >>
        done



-- --------------------------------------------------------------------------
-- SCALE
-- --------------------------------------------------------------------------           
testScale =
        newHBox []                                                      >>= \ b ->
        newScale [ interval (1,100),
--              text "second newScale",
                digits 0,
                increment (10::Double),
                height (cm 10),
                orient Horizontal,
                parent b
                ]                                               >>= \ cb2 ->
        configure (Slider cb2) [
                increment (1 :: Double),
                height (mm 5),
                width (cm 1),
                showValue On,
                repeatInterval 25,
                repeatDelay 75
                ]                                                >>
        newEntry [disable,pad Horizontal (cm 0.5), value (0:: Int), parent b, width 3]>>= \e ->
        window b [text "Test Scale"]                            >>= \ win2 ->
        controller win2 (\_ ->
                triggered cb2 >>>= \v -> value ((round v) :: Int) (e :: Entry Int) >> done) >>
        runScaletest cb2                                        >>      
        runSlidertest (Slider cb2)                              >>      
        done
        where runScaletest cb =
                testBaseWidget cb                               >>
                testHasColour cb                                        >>
                testHasSlider cb                                >>
                testHighlightedWidget cb                        >>
                testHasWidth cb                                 >>
                testHasHeight cb                                >>
                testHasFont cb                                  >>
                testHasOrientation cb                           >>
                fetchText cb                                    >>
                fetchC cb getDigits                             >>
                getInterval cb                                  >>
                fetchC cb (getIncrement :: Scale Double -> IO Double)   >>
                done
              runSlidertest cb =
                testHasWidth cb                                 >>
                testHasHeight cb                                >>
                fetchC cb  (getIncrement :: Slider (Scale Double) -> IO Double)                         >>
                fetchC cb getBigIncrement                       >>
                fetchC cb getShowValue                          >>
                done
        


-- --------------------------------------------------------------------------
-- SCROLLBAR
-- --------------------------------------------------------------------------           
testScrollBar =
        newHFBox []                                             >>= \ box ->
        newEditor [bg white,parent box, flexible, value ""]     >>= \ tp ->
        newScrollBar [parent box, fill Vertical]                        >>= \ cb ->
        window box [text "Test ScrollBar"]                      >>= \ win ->
        controller win (const inaction)                         >>
        configure tp [scrollbar Vertical cb]                    >>
        runScrollBarTest cb                                     >>
        done
        where runScrollBarTest cb =
                testBaseWidget cb                               >>
                testHasColour cb                                        >>
                testHasSlider cb                                >>
                testHighlightedWidget cb                        >>
                testHasWidth cb                                 >>
                testHasOrientation cb                           >>
                done


-- --------------------------------------------------------------------------
-- LISTBOX
-- --------------------------------------------------------------------------           
testListBox =
        newListBox []                                           >>= \ cb1 ->
        window cb1 [text "Test ListBox 1"]                      >>= \ win1 ->
        controller win1 (const inaction)                        >>
        runtest cb1                                             >>
        newListBox [value els,selectMode Multiple]              >>= \ cb2 ->
        window cb2 [text "Test ListBox"]                        >>= \ win2 ->
        testHasBBox cb2 (1::Int)                                >>
        controller win2 (const inaction)                        >>
        runtest cb2                                             >>
        updValue cb2 (++ ["black", "white"])                    >>
        configure cb2 [selection (ListBoxElem "green")]         >>
        getSelection cb2                                        >>= \ sel ->
        print (sel::Maybe [Int])                                >>
        clearSelection cb2                                      >>
        configure cb2 [selectionRange(ListBoxElem "blue") (ListBoxElem "red")]          >>
        try(selectionRange (ListBoxElem "B") (ListBoxElem "W") cb2)>>
        selectionRange (ListBoxElem "black") (ListBoxElem "white") cb2  >>
        done
        where els = ["blue","red","green","yellow","orange"]
              runtest :: ListBox [String] -> IO ()
              runtest cb = 
                testBaseWidget cb                               >>
                testHighlightedWidget cb                        >>
                testHasColour cb                                        >>
                testHasColumns cb                               >>
                testHasRows cb                                  >>
                testHasFont cb                                  >>
                testHasGrid cb                                  >>
                testSelection cb                                >>
                getValue cb                                     >>= \ el' ->
                value el' cb                            >>
                fetchC cb getSelectMode                         >>
                done

             

-- --------------------------------------------------------------------------
-- TEXT PANE
-- --------------------------------------------------------------------------           
testEditor =
        newEditor [wrap WordWrap, background white,flexible,value ""]   >>= \ cb ->
        window cb [text "Test Text Pane"]                       >>= \ win ->
        configure cb [height 40, width 60]                      >>
        controller win (const inaction)         >>= \ master ->
        testBaseWidget cb                                       >>
        testHasColour cb                                                >>
        testHighlightedWidget cb                                >>
        testHasColumns cb                                       >>
        testHasRows cb                                          >>
        testHasFont cb                                          >>
        testSelection cb                                        >>
        testHasICursor cb                                       >>
        testMultipleLinedWidget cb                              >>
        testHasBBox cb ((1,0) :: Position)                      >>
        fetchC cb getWrapMode                                   >>
        insertText cb (poss 1 0) "hello there"                  >>
        insertText cb (poss 2 0) "second newLine\n"             >>
        insertText cb (poss 3 0) "third newLine\n"              >>
        newTextTag cb (poss 1 0) (poss 1 1) [background red]    >>= \ t1 ->
        testTag t1                                              >>      
        newCounter      []                                              >>= \ cnt ->
        newEmbeddedTextWin cb cnt (poss 0 0) [align InCenter]   >>= \ ew ->
        testTextWindowItem ew                                   >>
        configure ew [align Bottom]                             >>
        controller cnt (\_ -> triggered cnt)                    >>= \ iact ->
        newLabel [bitmap questhead]                                     >>= \ bm ->
        newEmbeddedTextWin cb bm (poss 1 0) [align InCenter]            >>= \ wi ->
        testTextWindowItem wi                                   >>
        forkIOnull (walker bm questhead hourglass)                  >>
        testHasSelection cb (poss 0 1) (poss 1 1)                       >>
--      testHasInsertionCursor cb (poss 0 1)                    >>
--      testHasInsertionCursor cb (poss 1 1)                    >>
        done
        where walker :: Label BitMap -> BitMapHandle -> BitMapHandle -> IO ()
              walker bm bh1 bh2 =
                delay (secs 1)                                  >>
                configure bm [bitmap bh2]                               >>
                walker bm bh2 bh1


testTag t =
        testHasBorder t                                         >>
        testHasFont t                                           >>
        testHasJustify t                                        >>
        testMultipleLinedWidget t                               >>
        getBgstipple t                                          >>
        getFgstipple t                                          >>
        fetchC t getLmargin1                                    >>
        fetchC t getLmargin2                                    >>
        fetchC t getRmargin                                     >>
        fetchC t getOffset                                      >>
        fetchC t getOverstrike                                  >>
        fetchC t getUnderlined                                  >>
        done


-- --------------------------------------------------------------------------
-- EMBEDDED WINDOW ITEM
-- --------------------------------------------------------------------------           
testTextWindowItem cb =
        fetchC cb getStretch                                    >>
        fetchC cb getAlign                                      >>
        done
        

pos :: HasPosition w => Position -> Config w
pos = position

geo :: HasGeometry w => Geometry -> Config w
geo = geometry
                        

white = toColour "white"
blue = toColour "blue"
red = toColour "red"
green = toColour "green"


vwm win ev = destroyed win +> ev


wbbmap :: HasBitMap w => String -> Config w
wbbmap fnm w = do {
        prefix <- Posix.getEnvVar "WB_ROOT";
        configure w [bitmap (prefix ++ "/images/" ++ fnm)];
        return w
}


wbfilename :: HasFile w => String -> Config w
wbfilename fnm w = do {
        prefix <- Posix.getEnvVar "WB_ROOT";
        configure w [filename (prefix ++ "/images/" ++ fnm)];
        return w
}


(@>>) :: ( w -> IA a) -> IO () -> (w -> IA ())
f @>> c = \w -> f w >>> c

action :: GUIObject w => EventLoop GUIOBJECT -> (w -> IA ()) -> Config w
action el f w = do
        el # registerEH w  (f w)
        return w


instance GUIValue [Int] where
        cdefault = []

poss :: Distance -> Distance -> Position
poss x y = (x,y)

fetchText :: HasText w String => w ->  IO (Answer ())
fetchText w = fetchC w (getText :: HasText w String => w -> IO String)
